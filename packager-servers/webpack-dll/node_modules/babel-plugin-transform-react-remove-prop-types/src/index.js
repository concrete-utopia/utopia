// @flow weak
/* eslint-disable global-require, import/no-dynamic-require */

import { visitors } from 'babel-traverse';
// import generate from 'babel-generator';
// console.log(generate(node).code);
import isAnnotatedForRemoval from './isAnnotatedForRemoval';
import isStatelessComponent from './isStatelessComponent';
import remove from './remove';

function isPathReactClass(path) {
  if (path.matchesPattern('React.Component') ||
    path.matchesPattern('React.PureComponent')) {
    return true;
  }

  const node = path.node;

  if (node && (node.name === 'Component' || node.name === 'PureComponent')) {
    return true;
  }

  return false;
}

function isReactClass(superClass, scope) {
  if (!superClass.node) {
    return false;
  }

  let answer = false;

  if (isPathReactClass(superClass)) {
    answer = true;
  } else if (superClass.node.name) { // Check for inheritance
    const className = superClass.node.name;
    const binding = scope.getBinding(className);
    if (!binding) {
      answer = false;
    } else {
      superClass = binding.path.get('superClass');

      if (isPathReactClass(superClass)) {
        answer = true;
      }
    }
  }

  return answer;
}

export default function ({ template, types }) {
  return {
    visitor: {
      Program(programPath, state) {
        let ignoreFilenames;

        if (state.opts.ignoreFilenames) {
          ignoreFilenames = new RegExp(state.opts.ignoreFilenames.join('|'), 'gi');
        } else {
          ignoreFilenames = undefined;
        }

        const globalOptions = {
          visitedKey: `transform-react-remove-prop-types${Date.now()}`,
          unsafeWrapTemplate: template(`
            if (process.env.NODE_ENV !== "production") {
              NODE;
            }
          `),
          wrapTemplate: template(`
            LEFT = process.env.NODE_ENV !== "production" ? RIGHT : {}
          `),
          mode: state.opts.mode || 'remove',
          ignoreFilenames,
          types,
          removeImport: state.opts.removeImport || false,
          libraries: (state.opts.additionalLibraries || []).concat('prop-types'),
        };

        if (state.opts.plugins) {
          const pluginsVisitors = state.opts.plugins.map((pluginName) => {
            const plugin = require(pluginName);
            return plugin({ template, types }).visitor;
          });

          programPath.traverse(visitors.merge(pluginsVisitors));
        }

        // On program start, do an explicit traversal up front for this plugin.
        programPath.traverse({
          ObjectProperty: {
            exit(path) {
              const node = path.node;

              if (node.computed || node.key.name !== 'propTypes') {
                return;
              }

              const parent = path.findParent((currentNode) => {
                if (currentNode.type !== 'CallExpression') {
                  return false;
                }

                return currentNode.get('callee').node.name === 'createReactClass';
              });

              if (parent) {
                remove(path, globalOptions, {
                  type: 'createClass',
                });
              }
            },
          },
          // Here to support stage-1 transform-class-properties.
          ClassProperty(path) {
            const {
              node,
              scope,
            } = path;

            if (node.key.name === 'propTypes') {
              const pathClassDeclaration = scope.path;

              if (isReactClass(pathClassDeclaration.get('superClass'), scope)) {
                remove(path, globalOptions, {
                  type: 'class static',
                  pathClassDeclaration,
                });
              }
            }
          },
          AssignmentExpression(path) {
            const {
              node,
              scope,
            } = path;

            if (node.left.computed || !node.left.property || node.left.property.name !== 'propTypes') {
              return;
            }

            const forceRemoval = isAnnotatedForRemoval(path.node.left);

            if (forceRemoval) {
              remove(path, globalOptions, { type: 'assign' });
            }

            const className = node.left.object.name;
            const binding = scope.getBinding(className);

            if (!binding) {
              return;
            }

            if (binding.path.isClassDeclaration()) {
              const superClass = binding.path.get('superClass');

              if (isReactClass(superClass, scope)) {
                remove(path, globalOptions, { type: 'assign' });
              }
            } else if (isStatelessComponent(binding.path)) {
              remove(path, globalOptions, { type: 'assign' });
            }
          },
        });

        if (globalOptions.removeImport) {
          if (globalOptions.mode === 'remove') {
            programPath.scope.crawl();

            programPath.traverse({
              ImportDeclaration(path) {
                const { source, specifiers } = path.node;
                if (!globalOptions.libraries.includes(source.value)) {
                  return;
                }
                const haveUsedSpecifiers = specifiers.some((specifier) => {
                  const importedIdentifierName = specifier.local.name;
                  const { referencePaths } = path.scope.getBinding(importedIdentifierName);
                  return referencePaths.length > 0;
                });

                if (!haveUsedSpecifiers) {
                  path.remove();
                }
              },
            });
          } else {
            throw new Error(
              'react-remove-prop-types: removeImport and mode=remove can not be used at the same time.',
            );
          }
        }
      },
    },
  };
}
