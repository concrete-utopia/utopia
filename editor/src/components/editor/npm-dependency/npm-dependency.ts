import * as json5 from 'json5'
import * as R from 'ramda'
import { MapLike } from 'typescript'
import { UTOPIA_BACKEND } from '../../../common/env-vars'
import { sameCodeFile } from '../../../core/model/project-file-utils'
import {
  PossiblyUnversionedNpmDependency,
  PackageStatusMap,
  PackageStatus,
  RequestedNpmDependency,
  requestedNpmDependency,
  unversionedNpmDependency,
  resolvedNpmDependency,
} from '../../../core/shared/npm-dependency-types'
import {
  isCodeFile,
  Imports,
  ProjectContents,
  ProjectFile,
  NodeModules,
  esCodeFile,
  ImportDetails,
} from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import {
  EditorState,
  packageJsonFileFromProjectContents,
  updatePackageJsonInEditorState,
} from '../store/editor-state'
import { pluck } from '../../../core/shared/array-utils'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { useEditorState } from '../store/store-hook'
import * as React from 'react'
import { resolvedDependencyVersions } from '../../../core/third-party/third-party-components'
import { deepFreeze } from '../../../utils/deep-freeze'
import * as Semver from 'semver'
import { ProjectContentTreeRoot } from '../../assets'
import * as npa from 'npm-package-arg'
import * as GitHost from 'hosted-git-info'
import { importDefault, importStar } from '../../../core/es-modules/commonjs-interop'

interface PackageNotFound {
  type: 'PACKAGE_NOT_FOUND'
}

export const packageNotFound: PackageNotFound = deepFreeze({
  type: 'PACKAGE_NOT_FOUND',
})

interface PackageLookupSuccess {
  type: 'PACKAGE_LOOKUP_SUCCESS'
  json: unknown
}

function packageLookupSuccess(json: unknown): PackageLookupSuccess {
  return {
    type: 'PACKAGE_LOOKUP_SUCCESS',
    json: json,
  }
}

type PackageLookupResult = PackageNotFound | PackageLookupSuccess

type RequestedDependencyVersionType = 'GITHUB' | 'LOCAL' | 'SEMVER' | 'TAG' | 'URL'

interface NpmVersion {
  type: 'NPM_VERSION'
  version: string
}

interface HostedVersion {
  type: 'HOSTED_VERSION'
  gitHost: GitHost
  version: string
}

export function npmVersion(version: string): NpmVersion {
  return {
    type: 'NPM_VERSION',
    version: version,
  }
}

export function hostedVersion(gitHost: GitHost, version: string): HostedVersion {
  return {
    type: 'HOSTED_VERSION',
    gitHost: gitHost,
    version: version,
  }
}

export type ResolvedDependencyVersion = NpmVersion | HostedVersion

export function isNpmVersion(version: ResolvedDependencyVersion): version is NpmVersion {
  return version.type === 'NPM_VERSION'
}

export function isHostedVersion(version: ResolvedDependencyVersion): version is HostedVersion {
  return version.type === 'HOSTED_VERSION'
}

interface VersionLookupSuccess {
  type: 'VERSION_LOOKUP_SUCCESS'
  version: ResolvedDependencyVersion
}

export type VersionLookupResult = PackageNotFound | VersionLookupSuccess

export function npmVersionLookupSuccess(version: string): VersionLookupSuccess {
  return {
    type: 'VERSION_LOOKUP_SUCCESS',
    version: npmVersion(version),
  }
}

export function hostedVersionLookupSuccess(
  gitHost: GitHost,
  version: string,
): VersionLookupSuccess {
  return {
    type: 'VERSION_LOOKUP_SUCCESS',
    version: hostedVersion(gitHost, version),
  }
}

export function isPackageNotFound(
  lookupResult: PackageLookupResult | VersionLookupResult,
): lookupResult is PackageNotFound {
  return lookupResult.type === 'PACKAGE_NOT_FOUND'
}

function isPackageLookupSuccess(
  lookupResult: PackageLookupResult,
): lookupResult is PackageLookupSuccess {
  return lookupResult.type === 'PACKAGE_LOOKUP_SUCCESS'
}

export const findLatestVersion = (packageName: string): Promise<VersionLookupResult> =>
  fetchNpmMatchingVersion(packageName, 'latest')

function versionIsSemver(v: string): boolean {
  return Semver.validRange(v) != null // Don't. Just don't.
}

export function getVersionType(
  packageName: string,
  version: string,
): RequestedDependencyVersionType {
  try {
    const npaResolveResult = npa.resolve(packageName, version)
    switch (npaResolveResult.type) {
      case 'git':
        return 'GITHUB'
      case 'tag':
        return 'TAG'
      case 'version':
        return 'SEMVER'
      case 'range':
        return 'SEMVER'
      case 'file':
        return 'LOCAL'
      case 'directory':
        return 'LOCAL'
      case 'remote':
        return 'URL'
    }
  } catch (e) {
    /* Probably a typo or mid-typing; no need to throw here */
  }

  // If all else fails, assume it is a tagged version
  return 'TAG'
}

async function fetchNpmMatchingVersion(
  packageName: string,
  version: string,
): Promise<VersionLookupResult> {
  const encodedName = encodeURIComponent(packageName).replace(/^%40/, '@')
  const encodedVersion = encodeURIComponent(version)
  const response = await fetch(
    `${UTOPIA_BACKEND}javascript/package/versions/${encodedName}/${encodedVersion}`,
  )

  if (response.ok) {
    const matchingVersionsResult = await response.json()

    if (typeof matchingVersionsResult === 'string') {
      // Return result is a single concrete version
      return npmVersionLookupSuccess(matchingVersionsResult)
    } else if (
      typeof matchingVersionsResult === 'object' &&
      Array.isArray(matchingVersionsResult) &&
      versionIsSemver(version)
    ) {
      // Return result is an array of concrete versions, so we attempt to match the highest satsifying version
      const satisfyingVersion = Semver.maxSatisfying(matchingVersionsResult, version)
      return satisfyingVersion == null
        ? packageNotFound
        : npmVersionLookupSuccess(satisfyingVersion)
    } else {
      return Promise.reject(
        `Invalid matching versions result for ${packageName}@${version}: ${matchingVersionsResult}`,
      )
    }
  } else if (response.status === 404) {
    return packageNotFound
  } else {
    return Promise.reject(`Received an error for package ${packageName}@${version}`)
  }
}

export async function findMatchingVersion(
  packageName: string,
  version: string,
): Promise<VersionLookupResult> {
  const versionType = getVersionType(packageName, version)
  switch (versionType) {
    case 'GITHUB':
      const npaResolveResult = npa.resolve(packageName, version)
      if (npaResolveResult.hosted != null && npaResolveResult.gitRange == null) {
        // TODO Support semver ranges on hosted repos
        // npa appears to be using a buggy version of hosted-git-info
        const gitHost = GitHost.fromUrl(version)
        return Promise.resolve(hostedVersionLookupSuccess(gitHost, version))
      } else {
        throw new Error(`Unable to resolve host information for git hosted repo ${version}`)
      }
    case 'LOCAL':
      return Promise.resolve(packageNotFound)
    case 'SEMVER':
    case 'TAG':
      return fetchNpmMatchingVersion(packageName, version)
    case 'URL':
      return Promise.resolve(packageNotFound)
    default:
      const _exhaustiveCheck: never = versionType
      throw new Error(`Unhandled version type ${versionType}`)
  }
}

export async function checkPackageVersionExists(
  packageName: string,
  version: string,
): Promise<boolean> {
  const matchingVersion = await findMatchingVersion(packageName, version)
  switch (matchingVersion.type) {
    case 'VERSION_LOOKUP_SUCCESS':
      return true
    case 'PACKAGE_NOT_FOUND':
      return false
    default:
      const _exhaustiveCheck: never = matchingVersion
      throw new Error(`Unhandled version lookup type ${JSON.stringify(matchingVersion)}`)
  }
}

export function dependenciesFromPackageJsonContents(
  packageJson: string,
): Array<RequestedNpmDependency> {
  try {
    const parsedJSON = json5.parse(packageJson)

    const dependenciesJSON = R.path<any>(['dependencies'], parsedJSON)
    if (typeof dependenciesJSON === 'object') {
      let result: Array<RequestedNpmDependency> = []
      for (const dependencyKey of Object.keys(dependenciesJSON)) {
        const dependencyValue = dependenciesJSON[dependencyKey]
        if (typeof dependencyValue === 'string') {
          result.push(requestedNpmDependency(dependencyKey, dependencyValue))
        } else {
          return []
        }
      }
      return result
    } else {
      return []
    }
  } catch (error) {
    return []
  }
}

// Cache the dependencies when getting them from `EditorState` because lots of things need this information.
// IMPORTANT: This caching is relied upon indirectly by monaco-wrapper.tsx
interface PackageJsonAndDeps {
  packageJsonFile: ProjectFile
  npmDependencies: Array<RequestedNpmDependency>
}

let cachedDependencies: PackageJsonAndDeps | null = null

export function dependenciesFromPackageJson(
  packageJsonFile: ProjectFile | null,
): Array<RequestedNpmDependency> {
  if (packageJsonFile == null) {
    return []
  } else {
    if (
      cachedDependencies != null &&
      sameCodeFile(packageJsonFile, cachedDependencies.packageJsonFile)
    ) {
      return cachedDependencies.npmDependencies
    } else {
      if (isCodeFile(packageJsonFile)) {
        const npmDependencies = dependenciesFromPackageJsonContents(packageJsonFile.fileContents)
        if (npmDependencies == null) {
          cachedDependencies = null
        } else {
          cachedDependencies = {
            packageJsonFile: packageJsonFile,
            npmDependencies: npmDependencies,
          }
        }
        return npmDependencies
      } else {
        throw new Error(`Invalid file type for package.json: ${packageJsonFile.type}`)
      }
    }
  }
}

// Dependencies that the editor needs for code completion primarily.
// Effectively these are akin to `devDependencies` in `package.json`.
const EditorTypePackageDependencies: Array<RequestedNpmDependency> = [
  requestedNpmDependency('@types/react', '16.9.46'),
  requestedNpmDependency('@types/react-dom', '16.9.8'),
]

export function dependenciesWithEditorRequirements(
  projectContents: ProjectContentTreeRoot,
): Array<RequestedNpmDependency> {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const userDefinedDeps = dependenciesFromPackageJson(packageJsonFile)
  return [...userDefinedDeps, ...EditorTypePackageDependencies]
}

export function immediatelyResolvableDependenciesWithEditorRequirements(
  projectContents: ProjectContentTreeRoot,
): Array<PossiblyUnversionedNpmDependency> {
  const requestedDependencies = dependenciesWithEditorRequirements(projectContents)
  return requestedDependencies.map((requestedDependency) => {
    // As it satisfies the semver parse, we can (for now) assume
    // it's the version that will be resolved.
    if (Semver.parse(requestedDependency.version) == null) {
      return unversionedNpmDependency(requestedDependency.name)
    } else {
      return resolvedNpmDependency(requestedDependency.name, requestedDependency.version)
    }
  })
}

export function usePackageDependencies(): Array<RequestedNpmDependency> {
  const packageJsonFile = useEditorState((store) => {
    return packageJsonFileFromProjectContents(store.editor.projectContents)
  }, 'usePackageDependencies')

  return React.useMemo(() => {
    if (isCodeFile(packageJsonFile)) {
      return dependenciesFromPackageJsonContents(packageJsonFile.fileContents)
    } else {
      return []
    }
  }, [packageJsonFile])
}

export function usePossiblyResolvedPackageDependencies(): Array<PossiblyUnversionedNpmDependency> {
  const basePackageDependencies = usePackageDependencies()
  const files = useEditorState((store) => {
    return store.editor.nodeModules.files
  }, 'usePossiblyResolvedPackageDependencies')
  return React.useMemo(() => {
    return resolvedDependencyVersions(basePackageDependencies, files)
  }, [basePackageDependencies, files])
}

export function updateDependenciesInPackageJson(
  packageJson: string,
  npmDependencies: Array<RequestedNpmDependency>,
): string {
  function updateDeps(parsedPackageJson: any): string {
    return JSON.stringify(
      R.assocPath(['dependencies'], npmDependencies, parsedPackageJson),
      null,
      2,
    )
  }
  try {
    const parsedJSON = json5.parse(packageJson)
    return updateDeps(parsedJSON)
  } catch (error) {
    console.error('Error parsing package.json.', error)
    return updateDeps({})
  }
}

export function updateDependenciesInEditorState(
  editor: EditorState,
  npmDependencies: Array<RequestedNpmDependency>,
): EditorState {
  const transformPackageJson = (packageJson: string) => {
    return updateDependenciesInPackageJson(packageJson, npmDependencies)
  }
  return updatePackageJsonInEditorState(editor, transformPackageJson)
}

export function importResultFromModule(
  importDetails: ImportDetails,
  requireResult: any,
): MapLike<any> {
  let result: MapLike<any> = {}

  if (importDetails.importedWithName !== null) {
    // import name from './place'
    result[importDetails.importedWithName] = importDefault(requireResult)
  }
  if (importDetails.importedAs !== null) {
    // import * as name from './place'
    result[importDetails.importedAs] = importStar(requireResult)
  }
  Utils.fastForEach(importDetails.importedFromWithin, (fromWithin) => {
    result[fromWithin.alias] = requireResult[fromWithin.name]
  })

  return result
}

export function importResultFromImports(
  importOrigin: string,
  imports: Imports,
  requireFn: (origin: string, toImport: string) => any,
): MapLike<any> {
  let result: MapLike<any> = {}
  Utils.fastForEach(Object.keys(imports), (importSource) => {
    const requireResult = requireFn(importOrigin, importSource)
    if (requireResult == null) {
      console.warn(`Could not find ${importSource} with a require call.`)
    } else {
      result = {
        ...result,
        ...importResultFromModule(imports[importSource], requireResult),
      }
    }
  })
  return result
}

export function createLoadedPackageStatusMapFromDependencies(
  dependencies: Array<RequestedNpmDependency>,
  dependenciesWithErrors: Array<RequestedNpmDependency>,
  dependenciesNotFound: Array<RequestedNpmDependency>,
): PackageStatusMap {
  const errorDependencyNames = pluck(dependenciesWithErrors, 'name')
  const notFoundDependencyNames = pluck(dependenciesNotFound, 'name')
  return dependencies.reduce((statusMap: PackageStatusMap, dependency) => {
    const isError = errorDependencyNames.includes(dependency.name)
    const isNotFound = notFoundDependencyNames.includes(dependency.name)
    const status: PackageStatus = isError ? 'error' : isNotFound ? 'not-found' : 'loaded'
    statusMap[dependency.name] = {
      status: status,
    }
    return statusMap
  }, {})
}
