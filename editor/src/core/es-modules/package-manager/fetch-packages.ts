import * as GitHost from 'hosted-git-info'
import type { ResolvedDependencyVersion } from '../../../components/editor/npm-dependency/npm-dependency'
import {
  findMatchingVersion,
  isPackageNotFound,
} from '../../../components/editor/npm-dependency/npm-dependency'
import type { AnyJson } from '../../../missing-types/json'
import { parseStringToJSON } from '../../../utils/package-parser-utils'
import {
  appendToPath,
  getParentDirectory,
  getPartsFromPath,
  makePathFromParts,
} from '../../../utils/path-utils'
import type { ParseError, ParseResult } from '../../../utils/value-parser-utils'
import {
  objectKeyParser,
  optionalObjectKeyParser,
  parseString,
} from '../../../utils/value-parser-utils'
import { pluck } from '../../shared/array-utils'
import type { Either } from '../../shared/either'
import {
  applicative3Either,
  flatMapEither,
  foldEither,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
} from '../../shared/either'
import type {
  PackagerServerFile,
  PackagerServerResponse,
  RequestedNpmDependency,
} from '../../shared/npm-dependency-types'
import { objectMap } from '../../shared/object-utils'
import type {
  ESCodeFile,
  ESRemoteDependencyPlaceholder,
  NodeModuleFile,
  NodeModules,
} from '../../shared/project-file-types'
import { esCodeFile, esRemoteDependencyPlaceholder } from '../../shared/project-file-types'
import { isBuiltInDependency } from './built-in-dependencies'
import type { BuiltInDependencies } from './built-in-dependencies-list'
import { mangleNodeModulePaths, mergeNodeModules } from './merge-modules'
import { getJsDelivrFileUrl, getPackagerUrl } from './packager-url'
import {
  notifyOperationFinished,
  notifyOperationStarted,
} from '../../shared/import/import-operation-service'
import { ImportOperationResult } from '../../shared/import/import-operation-types'

let depPackagerCache: { [key: string]: PackagerServerResponse } = {}

const PACKAGES_TO_SKIP = ['utopia-api', 'react', 'react-dom', 'uuiui', 'uuiui-deps']
const NR_RETRIES = 3
const RETRY_FREQ_MS = process.env.JEST_WORKER_ID == undefined ? 10000 : 0

function getFileURLForPackageVersion(
  name: string,
  version: string,
  resolvedUrl: string | undefined,
  filePath: string,
): string {
  const gitHost = resolvedUrl == null ? null : GitHost.fromUrl(resolvedUrl)
  if (gitHost == null) {
    return getJsDelivrFileUrl(`${name}@${version}`, filePath)
  } else {
    return gitHost.file(filePath)
  }
}

function findPackageJsonForFile(filePath: string, response: PackagerServerResponse): string | null {
  const parentDir = getParentDirectory(filePath)
  const maybePackageJsonPath = appendToPath(parentDir, 'package.json')

  if (response.contents[maybePackageJsonPath] != null) {
    return maybePackageJsonPath
  } else if (parentDir === '/') {
    return null
  } else {
    return findPackageJsonForFile(parentDir, response)
  }
}

interface PackageJsonResolvedPackageFields {
  name: string
  version: string
  resolvedUrl: string | undefined
}

function parseNameVersionFromParsedPackageJson(
  parsedJSON: AnyJson,
): ParseResult<PackageJsonResolvedPackageFields> {
  return applicative3Either(
    (name, version, resolvedUrl) => {
      return {
        name: name,
        version: version,
        resolvedUrl: resolvedUrl,
      }
    },
    objectKeyParser(parseString, 'name')(parsedJSON),
    objectKeyParser(parseString, 'version')(parsedJSON),
    optionalObjectKeyParser(parseString, '_resolved')(parsedJSON),
  )
}

function parseNameVersionFromPackageJson(
  packageJsonContents: string,
): ParseResult<PackageJsonResolvedPackageFields> {
  const parsedJSON = parseStringToJSON(packageJsonContents)

  return flatMapEither(parseNameVersionFromParsedPackageJson, parsedJSON)
}

function packagerResponseFileToNodeModule(
  response: PackagerServerResponse,
  filePath: string,
  fileContentsOrPlaceholder: PackagerServerFile,
): NodeModuleFile {
  if (fileContentsOrPlaceholder === 'PLACEHOLDER_FILE') {
    const packageJsonFilePath = findPackageJsonForFile(filePath, response)
    const packageJsonPackagerServerFile =
      packageJsonFilePath == null ? null : response.contents[packageJsonFilePath]
    const packageJsonFileContent =
      packageJsonPackagerServerFile == null || packageJsonPackagerServerFile === 'PLACEHOLDER_FILE'
        ? null
        : packageJsonPackagerServerFile.content
    if (packageJsonFileContent == null || packageJsonFilePath == null) {
      return esCodeFile(
        `throw new Error('Failed to find package.json for file ${filePath}')`,
        'NODE_MODULES',
        filePath,
      )
    } else {
      const moduleDirPathLength = getPartsFromPath(packageJsonFilePath).length - 1
      const filePathParts = getPartsFromPath(filePath)
      const relativeFilePathParts = filePathParts.slice(moduleDirPathLength)
      const relativeFilePath = makePathFromParts(relativeFilePathParts)

      const parsedPackageJsonContent = parseNameVersionFromPackageJson(packageJsonFileContent)
      const fileUrlEither = mapEither(
        ({ name, version, resolvedUrl }) =>
          getFileURLForPackageVersion(name, version, resolvedUrl, relativeFilePath),
        parsedPackageJsonContent,
      )

      return foldEither<ParseError, string, NodeModuleFile>(
        (_) =>
          esCodeFile(
            `throw new Error('Failed to resolve file ${filePath}')`,
            'NODE_MODULES',
            filePath,
          ),
        (fileUrl) => esRemoteDependencyPlaceholder(fileUrl, false),
        fileUrlEither,
      )
    }
  } else {
    return esCodeFile(fileContentsOrPlaceholder.content, 'NODE_MODULES', filePath)
  }
}

export function extractNodeModulesFromPackageResponse(
  response: PackagerServerResponse,
): NodeModules {
  const extractFile = (fileContentsOrPlaceholder: PackagerServerFile, filePath: string) =>
    packagerResponseFileToNodeModule(response, filePath, fileContentsOrPlaceholder)
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

  return fetchPackagerResponseWithRetryInner(NR_RETRIES, RETRY_FREQ_MS)
}

async function fetchPackagerResponse(
  dependency: RequestedNpmDependency,
  resolvedVersion: ResolvedDependencyVersion,
): Promise<NodeModules | null> {
  if (PACKAGES_TO_SKIP.indexOf(dependency.name) > -1) {
    return null
  }

  if (window.KarmaTestEnvironment) {
    // prevent 404s caused by trying to hit a not running packager server in Karma tests
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
      const convertedResult = extractNodeModulesFromPackageResponse(resp)
      result = convertedResult
      // This result includes transitive dependencies too, but for all modules it only includes package.json, .js and .d.ts files.
      // All other file types are replaced with placeholders to save on downloading unnecessary files. Placeholders are then
      // downloaded when they are first needed
    } else {
      throw new Error('Packager response error')
    }
  } else {
    result = extractNodeModulesFromPackageResponse(depPackagerCache[packagesUrl])
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
  builtInDependencies: BuiltInDependencies,
  shouldRetry: boolean = true,
): Promise<NodeFetchResult> {
  const dependenciesToDownload = newDeps.filter(
    (d) => !isBuiltInDependency(builtInDependencies, d.name),
  )
  const nodeModulesArr = await Promise.all(
    dependenciesToDownload.map(
      async (newDep): Promise<Either<DependencyFetchError, NodeModules>> => {
        function notifyFetchEnd(result: ImportOperationResult) {
          notifyOperationFinished(
            {
              type: 'fetchDependency',
              id: `${newDep.name}@${newDep.version}`,
              dependencyName: newDep.name,
              dependencyVersion: newDep.version,
            },
            result,
          )
        }
        try {
          notifyOperationStarted({
            type: 'fetchDependency',
            id: `${newDep.name}@${newDep.version}`,
            dependencyName: newDep.name,
            dependencyVersion: newDep.version,
          })
          const matchingVersionResponse = await findMatchingVersion(
            newDep.name,
            newDep.version,
            'skipFetch',
          )
          if (isPackageNotFound(matchingVersionResponse)) {
            notifyFetchEnd(ImportOperationResult.Error)
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
            notifyFetchEnd(ImportOperationResult.Success)
            return right(mangleNodeModulePaths(newDep.name, packagerResponse))
          } else {
            notifyFetchEnd(ImportOperationResult.Error)
            return left(failError(newDep))
          }
        } catch (e) {
          // TODO: proper error handling, now we don't show error for a missing package. The error will be visible when you try to import
          notifyFetchEnd(ImportOperationResult.Error)
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
  const newFile: ESCodeFile = esCodeFile(responseAsString, 'NODE_MODULES', filepath)
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
