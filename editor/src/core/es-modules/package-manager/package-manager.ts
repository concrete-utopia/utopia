import type { NodeModules, ESCodeFile } from '../../shared/project-file-types'
import { isEsCodeFile, isEsRemoteDependencyPlaceholder } from '../../shared/project-file-types'
import type { RequireFn, TypeDefinitions } from '../../shared/npm-dependency-types'
import { resolveModule } from './module-resolution'
import { evaluator } from '../evaluator/evaluator'
import { fetchMissingFileDependency } from './fetch-packages'
import type { EditorDispatch } from '../../../components/editor/action-types'
import { memoize } from '../../shared/memoize'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { updateNodeModulesContents } from '../../../components/editor/actions/action-creators'
import { utopiaApiTypings } from './utopia-api-typings'
import { resolveBuiltInDependency } from './built-in-dependencies'
import type { ProjectContentTreeRoot } from '../../../components/assets'
import { applyLoaders } from '../../webpack-loaders/loaders'
import type { CurriedUtopiaRequireFn } from '../../../components/custom-code/code-file'
import type { BuiltInDependencies } from './built-in-dependencies-list'
import {
  isResolveNotPresent,
  isResolveSuccess,
  isResolveSuccessIgnoreModule,
} from './module-resolution-utils'
import type { FrameworkHooks } from '../../frameworks/framework-hooks'
import { getFrameworkHooks } from '../../frameworks/framework-hooks'

export interface FileEvaluationCache {
  exports: any
}

export function fileEvaluationCache(exports: any): FileEvaluationCache {
  return {
    exports: exports,
  }
}

export interface EvaluationCacheForPath {
  module: FileEvaluationCache
  lastEvaluatedContent: string
}

export function evaluationCacheForPath(
  module: FileEvaluationCache,
  lastEvaluatedContent: string,
): EvaluationCacheForPath {
  return {
    module: module,
    lastEvaluatedContent: lastEvaluatedContent,
  }
}

export type EvaluationCache = {
  [path: string]: EvaluationCacheForPath
}

export const DependencyNotFoundErrorName = 'DependencyNotFoundError'

export const ResolvingRemoteDependencyErrorName = 'ResolvingRemoteDependencyError'

export function createDependencyNotFoundError(importOrigin: string, toImport: string) {
  let error = new Error(`Could not find dependency: '${toImport}' relative to '${importOrigin}'`)
  error.name = DependencyNotFoundErrorName
  return error
}

export function createResolvingRemoteDependencyError(toImport: string) {
  let error = new Error(`Resolving remote dependency '${toImport}'...`)
  error.name = ResolvingRemoteDependencyErrorName
  return error
}

export const getCurriedEditorRequireFn = (
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
  evaluationCache: EvaluationCache,
  builtInDependencies: BuiltInDependencies,
): CurriedUtopiaRequireFn => {
  const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
    void moduleDownload.then((modulesToAdd: NodeModules) =>
      dispatch([updateNodeModulesContents(modulesToAdd)]),
    )
  }
  return (projectContents: ProjectContentTreeRoot) =>
    getRequireFn(
      onRemoteModuleDownload,
      projectContents,
      nodeModules,
      evaluationCache,
      builtInDependencies,
    )
}

export function getRequireFn(
  onRemoteModuleDownload: (moduleDownload: Promise<NodeModules>) => void,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  evaluationCache: EvaluationCache,
  builtInDependencies: BuiltInDependencies,
  injectedEvaluator = evaluator,
): RequireFn {
  const frameworkHooks: FrameworkHooks = getFrameworkHooks(projectContents)
  return function require(importOrigin, toImport): unknown {
    const builtInDependency = resolveBuiltInDependency(builtInDependencies, toImport)
    if (builtInDependency != null) {
      return builtInDependency
    }

    const resolveResult = resolveModule(projectContents, nodeModules, importOrigin, toImport)

    if (isResolveSuccessIgnoreModule(resolveResult)) {
      // we found an "ignored module" https://github.com/defunctzombie/package-browser-field-spec#ignore-a-module
      // the return value is an empty object
      return {}
    } else if (isResolveSuccess(resolveResult)) {
      const resolvedPath = resolveResult.success.path
      const resolvedFile = resolveResult.success.file

      if (isEsCodeFile(resolvedFile)) {
        const cacheEntryExists =
          resolvedPath in evaluationCache &&
          evaluationCache[resolvedPath].lastEvaluatedContent === resolvedFile.fileContents
        let fileCache: FileEvaluationCache
        if (cacheEntryExists) {
          fileCache = evaluationCache[resolvedPath].module
        } else {
          fileCache = {
            exports: {},
          }
          evaluationCache[resolvedPath] = {
            module: fileCache,
            lastEvaluatedContent: resolvedFile.fileContents,
          }
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
              fileCache,
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
        return fileCache.exports
      } else if (isEsRemoteDependencyPlaceholder(resolvedFile)) {
        if (!resolvedFile.downloadStarted) {
          // return empty exports object, fire off an async job to fetch the dependency from jsdelivr
          resolvedFile.downloadStarted = true
          const moduleDownload = fetchMissingFileDependency(resolvedFile, resolvedPath)
          onRemoteModuleDownload(moduleDownload)
        }

        throw createResolvingRemoteDependencyError(toImport)
      }
    } else if (isResolveNotPresent(resolveResult)) {
      const frameworkLookupPath = frameworkHooks.onResolveModuleNotPresent(
        projectContents,
        nodeModules,
        importOrigin,
        toImport,
      )
      if (frameworkLookupPath != null) {
        return require(importOrigin, frameworkLookupPath)
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
    matchesArg: Object.is, // for an object with thousands of entries, where the values are _large_ strings, even a shallow equals is expensive
  },
)
