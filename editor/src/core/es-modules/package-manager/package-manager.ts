import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from 'uuiui'
import * as UUIUIDeps from 'uuiui-deps'
import * as ImportedReact from 'react'
import * as ImportedReactDOM from 'react-dom'
import {
  NodeModules,
  isEsCodeFile,
  ESCodeFile,
  esCodeFile,
  isEsRemoteDependencyPlaceholder,
} from '../../shared/project-file-types'
import { RequireFn, TypeDefinitions } from '../../shared/npm-dependency-types'
import { resolveModule } from './module-resolution'
import { evaluator } from '../evaluator/evaluator'
import { fetchMissingFileDependency } from './fetch-packages'
import { EditorDispatch } from '../../../components/editor/action-types'
import { memoize } from '../../shared/memoize'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { updateNodeModulesContents } from '../../../components/editor/actions/actions'

export const DependencyNotFoundErrorName = 'DependencyNotFoundError'

export function createDependencyNotFoundError(importOrigin: string, toImport: string) {
  let error = new Error(`Could not find dependency: '${toImport}' relative to '${importOrigin}'`)
  error.name = DependencyNotFoundErrorName
  return error
}

export const FriendlyEsModuleErrorMessage = `This probably means that you tried to load an ES Module. Utopia doesn't currently support ES Modules. NPM probably has another variant of this module which supports Node Modules.`
export function createEsModuleError(filePath: string, error: Error) {
  return (error.message = `${error.message}

Error found in: ${filePath}

${FriendlyEsModuleErrorMessage}`)
}

function resolveBuiltinDependency(toImport: string): any | undefined {
  const React = ImportedReact
  const ReactDOM = ImportedReactDOM
  /**
   * DO NOT RELEASE THE SOFTWARE WITH THIS ENABLED
   * OR AT LEAST WITHOUT REVISITING THIS TOPIC
   * THIS IS FOR MAKING LOCAL DEVELOPMENT EASIER
   *
   * we are returning UtopiaAPI from the editor bundle, instead of the npm package bundle returned from the server here.
   * why? because this enables us to skip the bundler server and the bumping procedure while iterating on the utopia-api
   *
   * once the API is stable, we need to start versioning it, and then this hack will need to be removed
   * and importResultFromImports should be updated too
   */
  if (toImport === 'utopia-api') {
    return {
      ...UtopiaAPI,
      default: UtopiaAPI,
    }
  }

  if (toImport === 'uuiui') {
    return {
      ...UUIUI,
      default: UUIUI,
    }
  }

  if (toImport === 'uuiui-deps') {
    return {
      ...UUIUIDeps,
      default: UUIUIDeps,
    }
  }

  if (toImport === 'react') {
    return {
      ...React,
      default: React,
    }
  }

  if (toImport === 'react-dom') {
    return {
      ...ReactDOM,
      default: ReactDOM,
    }
  }
  return undefined
}

export const getMemoizedRequireFn = memoize(
  (nodeModules: NodeModules, dispatch: EditorDispatch) => {
    return getRequireFn(
      (modulesToAdd) => dispatch([updateNodeModulesContents(modulesToAdd, false)]),
      nodeModules,
    )
  },
  {
    maxSize: 1,
  },
)

export function getRequireFn(
  updateNodeModules: (modulesToAdd: NodeModules) => void,
  nodeModules: NodeModules,
  injectedEvaluator = evaluator,
): RequireFn {
  return function require(importOrigin, toImport): unknown {
    const builtinDependency = resolveBuiltinDependency(toImport)
    if (builtinDependency != null) {
      return builtinDependency
    }

    const resolvedPath = resolveModule(nodeModules, importOrigin, toImport)
    if (resolvedPath != null) {
      const resolvedFile = nodeModules[resolvedPath]
      if (resolvedFile != null && isEsCodeFile(resolvedFile)) {
        if (resolvedFile.evalResultCache == null) {
          try {
            /**
             * we create a result cache with an empty exports object here.
             * the `injectedEvaluator` function is going to mutate this exports object.
             * the reason is that if we have cyclic dependencies, we want to be able to
             * return a partial exports object for a module which is under evaluation,
             * to avoid infinite loops
             *
             * https://nodejs.org/api/modules.html#modules_cycles
             *
             */

            // TODO this is the node.js `module` object we pass in to the evaluation scope.
            // we should extend the module objects so it not only contains the exports,
            // to have feature parity with the popular bundlers (Parcel / webpack)
            let partialModule = {
              exports: {},
            }
            // MUTATION
            resolvedFile.evalResultCache = { module: partialModule }
            function partialRequire(name: string): unknown {
              return require(resolvedPath!, name)
            }
            injectedEvaluator(
              resolvedPath,
              resolvedFile.fileContents,
              resolvedFile.evalResultCache.module,
              partialRequire,
            )
          } catch (e) {
            /**
             * The module evaluation threw an error. We want to surface this error,
             * but before we do that, we want to clear out the evalResultCache
             * so the next time someone tries to run the same require,
             * we give another change to the evaluator.
             *
             * This is inline with the real Node behavior
             */
            // MUTATION
            resolvedFile.evalResultCache = null
            throw e
          }
        }
        return resolvedFile.evalResultCache.module.exports
      } else if (isEsRemoteDependencyPlaceholder(resolvedFile)) {
        if (!resolvedFile.downloadStarted) {
          // return empty exports object, fire off an async job to fetch the dependency from jsdelivr
          resolvedFile.downloadStarted = true
          fetchMissingFileDependency(updateNodeModules, resolvedFile, resolvedPath)
        }

        return {}
      }
    }
    throw createDependencyNotFoundError(importOrigin, toImport)
  }
}

export const getDependencyTypeDefinitions = memoize(
  (nodeModules: NodeModules): TypeDefinitions => {
    const dtsFilepaths = Object.keys(nodeModules).filter(
      (path) => path.endsWith('.d.ts') && isEsCodeFile(nodeModules[path]),
    )
    const ret = mapArrayToDictionary(
      dtsFilepaths,
      (filepath) => filepath,
      (filepath) => (nodeModules[filepath] as ESCodeFile).fileContents,
    )

    return ret
  },
  {
    maxSize: 1,
    equals: Object.is, // for an object with thousands of entries, where the values are _large_ strings, even a shallow equals is expensive
  },
)
