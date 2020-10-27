import {
  NodeModules,
  isEsCodeFile,
  ESCodeFile,
  isEsRemoteDependencyPlaceholder,
} from '../../shared/project-file-types'
import { RequireFn, TypeDefinitions } from '../../shared/npm-dependency-types'
import { isResolveSuccess, resolveModule } from './module-resolution'
import { evaluator } from '../evaluator/evaluator'
import { fetchMissingFileDependency } from './fetch-packages'
import { EditorDispatch } from '../../../components/editor/action-types'
import { memoize } from '../../shared/memoize'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { updateNodeModulesContents } from '../../../components/editor/actions/actions'
import { utopiaApiTypings } from './utopia-api-typings'
import { resolveBuiltInDependency } from './built-in-dependencies'
import { ProjectContentTreeRoot } from '../../../components/assets'
import { applyLoaders } from '../../webpack-loaders/loaders'

export const DependencyNotFoundErrorName = 'DependencyNotFoundError'

export function createDependencyNotFoundError(importOrigin: string, toImport: string) {
  let error = new Error(`Could not find dependency: '${toImport}' relative to '${importOrigin}'`)
  error.name = DependencyNotFoundErrorName
  return error
}

export const getEditorRequireFn = (
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
) => {
  const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
    // FIXME Update something in the state to show that we're downloading remote files
    moduleDownload.then((modulesToAdd: NodeModules) =>
      dispatch([updateNodeModulesContents(modulesToAdd, 'incremental')]),
    )
  }
  return getRequireFn(onRemoteModuleDownload, projectContents, nodeModules)
}

export function getRequireFn(
  onRemoteModuleDownload: (moduleDownload: Promise<NodeModules>) => void,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  injectedEvaluator = evaluator,
): RequireFn {
  return function require(importOrigin, toImport): unknown {
    const builtInDependency = resolveBuiltInDependency(toImport)
    if (builtInDependency != null) {
      return builtInDependency
    }

    const resolveResult = resolveModule(projectContents, nodeModules, importOrigin, toImport)
    if (isResolveSuccess(resolveResult)) {
      const resolvedPath = resolveResult.success.path
      const resolvedFile = resolveResult.success.file

      if (isEsCodeFile(resolvedFile)) {
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
            let partialModule = {
              exports: {},
            }
            function partialRequire(name: string): unknown {
              return require(resolvedPath, name)
            }

            // Apply the loaders to the raw file contents
            const loadedModuleResult = applyLoaders(resolvedPath, resolvedFile.fileContents)

            // TODO this is the node.js `module` object we pass in to the evaluation scope.
            // we should extend the module objects so it not only contains the exports,
            // to have feature parity with the popular bundlers (Parcel / webpack)
            // MUTATION
            resolvedFile.evalResultCache = { module: partialModule }
            injectedEvaluator(
              loadedModuleResult.filename,
              loadedModuleResult.loadedContents,
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
          const moduleDownload = fetchMissingFileDependency(resolvedFile, resolvedPath)
          onRemoteModuleDownload(moduleDownload)
        }

        return {} // FIXME Throw or otherwise block further evaluation here
      }
    }
    throw createDependencyNotFoundError(importOrigin, toImport)
  }
}

// These are used for code completion
const UtopiaProvidedTypings = {
  '/node_modules/utopia-api/index.d.ts': utopiaApiTypings,
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

    return {
      ...UtopiaProvidedTypings,
      ...ret,
    }
  },
  {
    maxSize: 1,
    equals: Object.is, // for an object with thousands of entries, where the values are _large_ strings, even a shallow equals is expensive
  },
)
