import {
  NodeModules,
  esCodeFile,
  esRemoteDependencyPlaceholder,
  ESRemoteDependencyPlaceholder,
  ESCodeFile,
  NodeModuleFile,
} from '../../shared/project-file-types'
import {
  PackagerServerResponse,
  JsdelivrResponse,
  RequestedNpmDependency,
  ResolvedNpmDependency,
  resolvedNpmDependency,
  PackagerServerFile,
} from '../../shared/npm-dependency-types'
import { mapArrayToDictionary, pluck } from '../../shared/array-utils'
import { objectMap } from '../../shared/object-utils'
import { mangleNodeModulePaths, mergeNodeModules } from './merge-modules'
import { getPackagerUrl, getJsDelivrFileUrl } from './packager-url'
import { Either, right, left, isLeft, isRight } from '../../shared/either'
import { isBuiltInDependency } from './built-in-dependencies'
import {
  findMatchingVersion,
  isNpmVersion,
  isPackageNotFound,
  ResolvedDependencyVersion,
} from '../../../components/editor/npm-dependency/npm-dependency'

let depPackagerCache: { [key: string]: PackagerServerResponse } = {}

const PACKAGES_TO_SKIP = ['utopia-api', 'react', 'react-dom', 'uuiui', 'uuiui-deps']
const NR_RETRIES = 3
const RETRY_FREQ_MS = process.env.JEST_WORKER_ID == undefined ? 10000 : 0

function extractFilePath(packagename: string, filepath: string): string {
  const packagenameIndex = filepath.indexOf(packagename)
  const restOfFileUrl = filepath.slice(packagenameIndex + packagename.length)
  return restOfFileUrl
}

function getFileURLForPackageVersion(
  packageName: string,
  packageVersion: ResolvedDependencyVersion,
  filePath: string,
): string {
  const version = packageVersion.version
  const localFilePath = extractFilePath(packageName, filePath)
  if (isNpmVersion(packageVersion)) {
    return getJsDelivrFileUrl(`${packageName}@${version}`, localFilePath)
  } else {
    return packageVersion.gitHost.file(localFilePath)
  }
}

function packagerResponseFileToNodeModule(
  packageName: string,
  packageVersion: ResolvedDependencyVersion,
  filePath: string,
  fileContentsOrPlaceholder: PackagerServerFile,
): NodeModuleFile {
  if (fileContentsOrPlaceholder === 'PLACEHOLDER_FILE') {
    const fileUrl = getFileURLForPackageVersion(packageName, packageVersion, filePath)
    return esRemoteDependencyPlaceholder(fileUrl, false)
  } else {
    return esCodeFile(fileContentsOrPlaceholder.content, null)
  }
}

export function extractNodeModulesFromPackageResponse(
  packageName: string,
  packageVersion: ResolvedDependencyVersion,
  response: PackagerServerResponse,
): NodeModules {
  const extractFile = (fileContentsOrPlaceholder: PackagerServerFile, filePath: string) =>
    packagerResponseFileToNodeModule(
      packageName,
      packageVersion,
      filePath,
      fileContentsOrPlaceholder,
    )
  return objectMap(extractFile, response.contents)
}

export function resetDepPackagerCache() {
  depPackagerCache = {}
}

function toVersionedDependencyString(
  dependency: RequestedNpmDependency,
  resolvedVersion: ResolvedDependencyVersion,
): string {
  switch (resolvedVersion.type) {
    case 'NPM_VERSION':
      return `${dependency.name}@${resolvedVersion.version}`
    case 'HOSTED_VERSION':
      return resolvedVersion.version
    default:
      const _exhaustiveCheck: never = resolvedVersion
      throw new Error(`Unhandled package version type ${JSON.stringify(resolvedVersion)}`)
  }
}

async function fetchPackagerResponseWithRetry(
  dependency: RequestedNpmDependency,
  resolvedVersion: ResolvedDependencyVersion,
): Promise<NodeModules | null> {
  const wait = (ms: number) => {
    return new Promise((resolve) => setTimeout(resolve, ms))
  }

  const fetchPackagerResponseWithRetryInner = async (
    nrRetries: number,
    retryFreqMs: number,
  ): Promise<NodeModules | null> => {
    try {
      return await fetchPackagerResponse(dependency, resolvedVersion)
    } catch (e) {
      if (nrRetries < 1) {
        throw e
      }
      await wait(retryFreqMs)
      return await fetchPackagerResponseWithRetryInner(nrRetries - 1, retryFreqMs)
    }
  }

  return await fetchPackagerResponseWithRetryInner(NR_RETRIES, RETRY_FREQ_MS)
}

async function fetchPackagerResponse(
  dependency: RequestedNpmDependency,
  resolvedVersion: ResolvedDependencyVersion,
): Promise<NodeModules | null> {
  if (PACKAGES_TO_SKIP.indexOf(dependency.name) > -1) {
    return null
  }

  const versionedDependency = toVersionedDependencyString(dependency, resolvedVersion)

  const packagesUrl = getPackagerUrl(versionedDependency)
  let result: NodeModules = {}
  if (depPackagerCache[packagesUrl] == null) {
    const packagerResponse = await fetch(packagesUrl)
    if (packagerResponse.ok) {
      const resp = (await packagerResponse.json()) as PackagerServerResponse
      depPackagerCache[packagesUrl] = resp
      const convertedResult = extractNodeModulesFromPackageResponse(
        dependency.name,
        resolvedVersion,
        resp,
      )
      result = convertedResult
      // This result includes transitive dependencies too, but for all modules it only includes package.json, .js and .d.ts files.
      // All other file types are replaced with placeholders to save on downloading unnecessary files. Placeholders are then
      // downloaded when they are first needed
    } else {
      throw new Error('Packager response error')
    }
  } else {
    result = extractNodeModulesFromPackageResponse(
      dependency.name,
      resolvedVersion,
      depPackagerCache[packagesUrl],
    )
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
  const dependenciesToDownload = newDeps.filter((d) => !isBuiltInDependency(d.name))
  const nodeModulesArr = await Promise.all(
    dependenciesToDownload.map(
      async (newDep): Promise<Either<DependencyFetchError, NodeModules>> => {
        try {
          const matchingVersionResponse = await findMatchingVersion(newDep.name, newDep.version)
          if (isPackageNotFound(matchingVersionResponse)) {
            return left(failNotFound(newDep))
          }

          const fetchResolvedDependency = shouldRetry
            ? fetchPackagerResponseWithRetry
            : fetchPackagerResponse

          const packagerResponse = await fetchResolvedDependency(
            newDep,
            matchingVersionResponse.version,
          )

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
            return right(mangleNodeModulePaths(newDep.name, packagerResponse))
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

export async function fetchMissingFileDependency(
  dependency: ESRemoteDependencyPlaceholder,
  filepath: string,
): Promise<NodeModules> {
  const jsdelivrResponse = await fetch(dependency.url)
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

  return nodeModulesNewEntry
}
