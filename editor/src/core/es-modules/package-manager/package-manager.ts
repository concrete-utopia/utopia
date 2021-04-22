import {
  NodeModules,
  isEsCodeFile,
  ESCodeFile,
  isEsRemoteDependencyPlaceholder,
} from '../../shared/project-file-types'
import { RequireFn, TypeDefinitions } from '../../shared/npm-dependency-types'
import { isResolveSuccess, resolveModule, resolveModulePath } from './module-resolution'
import { evaluator } from '../evaluator/evaluator'
import { fetchMissingFileDependency } from './fetch-packages'
import { EditorDispatch } from '../../../components/editor/action-types'
import { memoize } from '../../shared/memoize'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { updateNodeModulesContents } from '../../../components/editor/actions/action-creators'
import { utopiaApiTypings } from './utopia-api-typings'
import { resolveBuiltInDependency } from './built-in-dependencies'
import { ProjectContentTreeRoot } from '../../../components/assets'
import { applyLoaders } from '../../webpack-loaders/loaders'
import { string } from 'prop-types'
import { Either } from '../../shared/either'

export type FileEvaluationCache = { exports: any }

export type EvaluationCache = { [path: string]: FileEvaluationCache }

export const DependencyNotFoundErrorName = 'DependencyNotFoundError'

export function createDependencyNotFoundError(importOrigin: string, toImport: string) {
  let error = new Error(`Could not find dependency: '${toImport}' relative to '${importOrigin}'`)
  error.name = DependencyNotFoundErrorName
  return error
}

export const getEditorResolveFunction = (
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
) => (importOrigin: string, toImport: string): Either<string, string> =>
  resolveModulePath(projectContents, nodeModules, importOrigin, toImport)

export const getEditorRequireFn = (
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
  evaluationCache: EvaluationCache,
): RequireFn => {
  const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
    // FIXME Update something in the state to show that we're downloading remote files
    moduleDownload.then((modulesToAdd: NodeModules) =>
      dispatch([updateNodeModulesContents(modulesToAdd, 'incremental')]),
    )
  }
  return getRequireFn(onRemoteModuleDownload, projectContents, nodeModules, evaluationCache, false)
}

export function getRequireFn(
  onRemoteModuleDownload: (moduleDownload: Promise<NodeModules>) => void,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  evaluationCache: EvaluationCache,
  isPreview: boolean,
  injectedEvaluator = evaluator,
): RequireFn {
  return function require(importOrigin, toImport): unknown {
    const builtInDependency = resolveBuiltInDependency(toImport, isPreview)
    if (builtInDependency != null) {
      return builtInDependency
    }

    const resolveResult = resolveModule(projectContents, nodeModules, importOrigin, toImport)
    if (isResolveSuccess(resolveResult)) {
      const resolvedPath = resolveResult.success.path
      const resolvedFile = resolveResult.success.file

      if (isEsCodeFile(resolvedFile)) {
        const cacheEntryExists = resolvedPath in evaluationCache
        let fileEvaluationCache: FileEvaluationCache
        if (cacheEntryExists) {
          fileEvaluationCache = evaluationCache[resolvedPath]
        } else {
          fileEvaluationCache = {
            exports: {},
          }
          evaluationCache[resolvedPath] = fileEvaluationCache
        }
        if (!cacheEntryExists) {
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
            function partialRequire(name: string): unknown {
              return require(resolvedPath, name)
            }

            // Apply the loaders to the raw file contents
            const loadedModuleResult = applyLoaders(resolvedPath, resolvedFile.fileContents)

            // TODO this is the node.js `module` object we pass in to the evaluation scope.
            // we should extend the module objects so it not only contains the exports,
            // to have feature parity with the popular bundlers (Parcel / webpack)
            // MUTATION
            injectedEvaluator(
              loadedModuleResult.filename,
              loadedModuleResult.loadedContents,
              fileEvaluationCache,
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
            delete evaluationCache[resolvedPath]
            throw e
          }
        }
        return fileEvaluationCache.exports
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
