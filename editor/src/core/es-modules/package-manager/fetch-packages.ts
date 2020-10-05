import {
  NodeModules,
  esCodeFile,
  esRemoteDependencyPlaceholder,
  ESRemoteDependencyPlaceholder,
  ESCodeFile,
} from '../../shared/project-file-types'
import {
  PackagerServerResponse,
  JsdelivrResponse,
  RequestedNpmDependency,
  ResolvedNpmDependency,
  resolvedNpmDependency,
} from '../../shared/npm-dependency-types'
import { mapArrayToDictionary, pluck } from '../../shared/array-utils'
import { objectMap } from '../../shared/object-utils'
import { mangleNodeModulePaths, mergeNodeModules } from './merge-modules'
import { getPackagerUrl, getJsDelivrListUrl, getJsDelivrFileUrl } from './packager-url'
import { Either, right, left, isLeft, isRight } from '../../shared/either'
import { isBuiltinDependency } from './package-manager'
import {
  findMatchingVersion,
  isPackageNotFound,
} from '../../../components/editor/npm-dependency/npm-dependency'
import { parseDependencyVersionFromNodeModules } from '../../../utils/package-parser-utils'

let depPackagerCache: { [key: string]: PackagerServerResponse } = {}
let jsDelivrCache: { [key: string]: JsdelivrResponse } = {}

const PACKAGES_TO_SKIP = ['utopia-api', 'react', 'react-dom', 'uuiui', 'uuiui-deps']
const NR_RETRIES = 3
const RETRY_FREQ_MS = process.env.JEST_WORKER_ID == undefined ? 10000 : 0

function extractNodeModulesFromPackageResponse(response: PackagerServerResponse): NodeModules {
  return objectMap((file) => {
    return esCodeFile(file.content, null)
  }, response.contents)
}

function extractNodeModulesFromJsdelivrResponse(
  packageName: string,
  version: string,
  response: JsdelivrResponse,
): NodeModules {
  const filteredFiles = response.files
  return mapArrayToDictionary(
    filteredFiles,
    (file) => `/node_modules/${packageName}${file.name}`,
    (file) => esRemoteDependencyPlaceholder(packageName, version, false),
  )
}

export function resetDepPackagerCache() {
  depPackagerCache = {}
}

async function fetchPackagerResponseWithRetry(
  dep: ResolvedNpmDependency,
): Promise<NodeModules | null> {
  const wait = (ms: number) => {
    return new Promise((resolve) => setTimeout(resolve, ms))
  }

  const fetchPackagerResponseWithRetryInner = async (
    dependency: ResolvedNpmDependency,
    nrRetries: number,
    retryFreqMs: number,
  ): Promise<NodeModules | null> => {
    try {
      return await fetchPackagerResponse(dependency)
    } catch (e) {
      if (nrRetries < 1) {
        throw e
      }
      await wait(retryFreqMs)
      return await fetchPackagerResponseWithRetryInner(dependency, nrRetries - 1, retryFreqMs)
    }
  }

  return await fetchPackagerResponseWithRetryInner(dep, NR_RETRIES, RETRY_FREQ_MS)
}

async function fetchPackagerResponse(dep: ResolvedNpmDependency): Promise<NodeModules | null> {
  if (PACKAGES_TO_SKIP.indexOf(dep.name) > -1) {
    return null
  }

  const packagesUrl = getPackagerUrl(dep)
  let result: NodeModules = {}
  if (depPackagerCache[packagesUrl] == null) {
    const packagerResponse = await fetch(packagesUrl)
    if (packagerResponse.ok) {
      const resp = (await packagerResponse.json()) as PackagerServerResponse
      depPackagerCache[packagesUrl] = resp
      const convertedResult = extractNodeModulesFromPackageResponse(resp)
      result = convertedResult
    } else {
      throw new Error('Packager response error')
    }
  } else {
    result = extractNodeModulesFromPackageResponse(depPackagerCache[packagesUrl])
  }

  const jsdelivrUrl = getJsDelivrListUrl(dep)
  if (jsDelivrCache[jsdelivrUrl] != null) {
    result = {
      ...extractNodeModulesFromJsdelivrResponse(dep.name, dep.version, jsDelivrCache[jsdelivrUrl]),
      ...result,
    }
  } else {
    const jsdelivrResponse = await fetch(jsdelivrUrl)
    if (jsdelivrResponse.ok) {
      const resp = (await jsdelivrResponse.json()) as JsdelivrResponse
      jsDelivrCache[jsdelivrUrl] = resp
      const nodeModulesFromResp = extractNodeModulesFromJsdelivrResponse(
        dep.name,
        dep.version,
        resp,
      )
      result = {
        ...nodeModulesFromResp, // we are deliberately merging this as the lower priority
        ...result, // because if there's a real .js or .d.ts, that should win
      }
    }
  }
  // Note: no error management, imports will show an error
  return result
}

export interface NodeFetchResult {
  dependenciesWithError: Array<RequestedNpmDependency>
  dependenciesNotFound: Array<RequestedNpmDependency>
  nodeModules: NodeModules
}

interface DependencyFetchError {
  type: 'FAIL_ERROR' | 'FAIL_NOT_FOUND'
  dependency: RequestedNpmDependency
}

function failNotFound(dependency: RequestedNpmDependency): DependencyFetchError {
  return {
    type: 'FAIL_NOT_FOUND',
    dependency: dependency,
  }
}

function failError(dependency: RequestedNpmDependency): DependencyFetchError {
  return {
    type: 'FAIL_ERROR',
    dependency: dependency,
  }
}

export async function fetchNodeModules(
  newDeps: Array<RequestedNpmDependency>,
  shouldRetry: boolean = true,
): Promise<NodeFetchResult> {
  const dependenciesToDownload = newDeps.filter((d) => !isBuiltinDependency(d.name))
  const nodeModulesArr = await Promise.all(
    dependenciesToDownload.map(
      async (newDep): Promise<Either<DependencyFetchError, NodeModules>> => {
        try {
          const matchingVersionResponse = await findMatchingVersion(newDep.name, newDep.version)
          if (isPackageNotFound(matchingVersionResponse)) {
            return left(failNotFound(newDep))
          }

          const resolvedDependency = resolvedNpmDependency(
            newDep.name,
            matchingVersionResponse.version,
          )

          const packagerResponse = shouldRetry
            ? await fetchPackagerResponseWithRetry(resolvedDependency)
            : await fetchPackagerResponse(resolvedDependency)
          if (packagerResponse != null) {
            /**
             * to avoid clashing transitive dependencies,
             * we "move" all transitive dependencies into a subfolder at
             * /node_modules/<main_package>/node_modules/<transitive_dep>/
             *
             * the module resolution won't mind this, the only downside to this approach is
             * that if two main dependencies share the exact same version of a transitive
             * dependency, they will not share that transitive dependency in memory,
             * so this is wasting a bit of memory.
             *
             * but it avoids two of the same transitive dependencies with different versions from
             * overwriting each other.
             *
             * the real nice solution would be to apply npm's module resolution logic that
             * pulls up shared transitive dependencies to the main /node_modules/ folder.
             */
            return right(mangleNodeModulePaths(resolvedDependency.name, packagerResponse))
          } else {
            return left(failError(newDep))
          }
        } catch (e) {
          // TODO: proper error handling, now we don't show error for a missing package. The error will be visible when you try to import
          return left(failError(newDep))
        }
      },
    ),
  )
  const errors = nodeModulesArr
    .filter(isLeft)
    .filter((e) => e.value.type === 'FAIL_ERROR')
    .map((e) => e.value.dependency)
  const notFound = nodeModulesArr
    .filter(isLeft)
    .filter((e) => e.value.type === 'FAIL_NOT_FOUND')
    .map((e) => e.value.dependency)
  const successes = nodeModulesArr.filter(isRight)
  const nodeModules = mergeNodeModules(pluck(successes, 'value'))
  return {
    dependenciesWithError: errors,
    dependenciesNotFound: notFound,
    nodeModules: nodeModules,
  }
}

function extractFilePath(packagename: string, filepath: string): string {
  const packagenameIndex = filepath.indexOf(packagename)
  const restOfFileUrl = filepath.slice(packagenameIndex + packagename.length)
  return restOfFileUrl
}

export async function fetchMissingFileDependency(
  updateNodeModules: (modulesToAdd: NodeModules) => void,
  dependency: ESRemoteDependencyPlaceholder,
  filepath: string,
): Promise<void> {
  const localFilePath = extractFilePath(dependency.packagename, filepath)
  const jsdelivrUrl = getJsDelivrFileUrl(
    resolvedNpmDependency(dependency.packagename, dependency.version),
    localFilePath,
  )
  const jsdelivrResponse = await fetch(jsdelivrUrl)
  const responseAsString = await jsdelivrResponse.text()
  const newFile: ESCodeFile = esCodeFile(responseAsString, null)
  const nodeModulesNewEntry: NodeModules = {
    [filepath]: newFile,
  }
  /**
   * we flip dependency.downloadStarted back to false. the reason is that there might be a race condition
   * between saving the nodeModules that triggered this fetchMissingFileDependency and saving the updateNodeModules itself.
   * in case the other nodeModules wins, we want to re-run fetchMissingFileDependency, to be able to dispatch updateNodeModules for a second time.
   * this solution is not nice and I hope we can remove it soon
   */
  // MUTATION
  dependency.downloadStarted = false
  updateNodeModules(nodeModulesNewEntry)
}
