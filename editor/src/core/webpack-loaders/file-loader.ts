// Emulates the behaviour of https://webpack.js.org/loaders/file-loader/ using the matchers from create-react-app
// https://github.com/facebook/create-react-app/blob/1f2d387db621ed9b0dbb62c36676d191942197d7/packages/react-scripts/config/webpack.config.js

import * as BabelTraverse from '@babel/traverse'
import * as BabelTypes from '@babel/types'
import { variableDeclarationsForImport } from './babel-helpers'
import { directoryForFilePath } from '../shared/file-utils'

const include = [/\.avif$/, /\.bmp$/, /\.gif$/, /\.jpe?g$/, /\.png$/]

function moduleDefaultExportForUrl(url: string): BabelTypes.Literal {
  return BabelTypes.stringLiteral(url)
}

function moduleCodeForUrl(url: string): BabelTypes.ObjectExpression {
  // { default: url }

  return BabelTypes.objectExpression([
    BabelTypes.objectProperty(BabelTypes.identifier('default'), moduleDefaultExportForUrl(url)),
  ])
}

export function FileLoaderPlugin(
  currentFilePath: string,
): (
  babel: any,
) => {
  visitor: BabelTraverse.Visitor
} {
  const directory = directoryForFilePath(currentFilePath)
  const urlForPath = (path: string) => `.${directory}/${path}` // We have to append a '.' to treat this as a relative path

  return () => {
    return {
      visitor: {
        Identifier(path) {
          const node = path.node
          if (node.name === 'require') {
            const parent = path.parent
            if (parent.type === 'CallExpression') {
              const args = parent.arguments
              if (args.length === 1 && args[0].type === 'StringLiteral') {
                const importFilePath = args[0].value
                const isMatchingPath = include.some((matcher) => matcher.test(importFilePath))
                if (isMatchingPath) {
                  const replacement = moduleDefaultExportForUrl(urlForPath(importFilePath))
                  path.parentPath.replaceWith(replacement)
                }
              }
            }
          }
        },
        ImportDeclaration(path) {
          const node = path.node
          const importFilePath = node.source.value
          const isMatchingPath = include.some((matcher) => matcher.test(importFilePath))
          if (isMatchingPath) {
            const replacement = variableDeclarationsForImport(
              node,
              moduleCodeForUrl(urlForPath(importFilePath)),
            )
            path.replaceWithMultiple(replacement)
          }
        },
      },
    }
  }
}
