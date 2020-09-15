import * as json5 from 'json5'
import * as R from 'ramda'
import { MapLike } from 'typescript'
import { UTOPIA_BACKEND } from '../../../common/env-vars'
import { sameCodeFile } from '../../../core/model/project-file-utils'
import {
  NpmDependency,
  npmDependency,
  PossiblyUnversionedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../../core/shared/npm-dependency-types'
import {
  isCodeFile,
  Imports,
  ProjectContents,
  ProjectFile,
  NodeModules,
  esCodeFile,
} from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import {
  EditorState,
  packageJsonFileFromProjectContents,
  updatePackageJsonInEditorState,
} from '../store/editor-state'
import { objectMap } from '../../../core/shared/object-utils'
import { mapArrayToDictionary, pluck } from '../../../core/shared/array-utils'
import { useEditorState } from '../store/store-hook'
import * as React from 'react'
import { resolvedDependencyVersions } from '../../../core/third-party/third-party-components'
import { deepFreeze } from '../../../utils/deep-freeze'

interface PackageNotFound {
  type: 'PACKAGE_NOT_FOUND'
}

const packageNotFound: PackageNotFound = deepFreeze({
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

interface VersionLookupSuccess {
  type: 'VERSION_LOOKUP_SUCCESS'
  version: string
}

function versionLookupSuccess(version: string): VersionLookupSuccess {
  return {
    type: 'VERSION_LOOKUP_SUCCESS',
    version: version,
  }
}

export type VersionLookupResult = PackageNotFound | VersionLookupSuccess

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

async function packageLookupCall(
  packageName: string,
  packageVersion: string | null,
): Promise<PackageLookupResult> {
  const requestInit: RequestInit = {
    headers: {
      accept: 'application/vnd.npm.install-v1+json; q=1.0, application/json; q=0.8, */*',
    },
  }

  const encodedName = encodeURIComponent(packageName).replace(/^%40/, '@')
  const URLSuffix = packageVersion == null ? encodedName : `${encodedName}/${packageVersion}`

  // Calls out to our services because of the wonder of CORS.
  const response = await fetch(
    `${UTOPIA_BACKEND}javascript/package/metadata/${URLSuffix}`,
    requestInit,
  )
  if (response.ok) {
    const json = await response.json()
    return packageLookupSuccess(json)
  } else if (response.status === 404) {
    return packageNotFound
  } else {
    const packageNameAndVersion =
      packageVersion == null ? packageName : `${packageName}@${packageVersion}`

    return Promise.reject(`Received an error for package ${packageNameAndVersion}`)
  }
}

export async function findLatestVersion(packageName: string): Promise<VersionLookupResult> {
  const metadata = await packageLookupCall(packageName, null)
  switch (metadata.type) {
    case 'PACKAGE_LOOKUP_SUCCESS':
      const latestVersion = Utils.path(['json', 'dist-tags', 'latest'], metadata)
      if (latestVersion == null || typeof latestVersion != 'string') {
        return Promise.reject(`Received invalid content for package ${packageName}`)
      } else {
        return Promise.resolve(versionLookupSuccess(latestVersion))
      }
    case 'PACKAGE_NOT_FOUND':
      return packageNotFound
    default:
      const _exhaustiveCheck: never = metadata
      throw new Error(`Unhandled package lookup type ${JSON.stringify(metadata)}`)
  }
}

export async function checkPackageVersionExists(
  packageName: string,
  version: string,
): Promise<VersionLookupResult> {
  const metadata = await packageLookupCall(packageName, version)
  switch (metadata.type) {
    case 'PACKAGE_LOOKUP_SUCCESS':
      return versionLookupSuccess(version)
    case 'PACKAGE_NOT_FOUND':
      return packageNotFound
    default:
      const _exhaustiveCheck: never = metadata
      throw new Error(`Unhandled package lookup type ${JSON.stringify(metadata)}`)
  }
}

export function dependenciesFromPackageJsonContents(packageJson: string): Array<NpmDependency> {
  try {
    const parsedJSON = json5.parse(packageJson)

    const dependenciesJSON = R.path<any>(['dependencies'], parsedJSON)
    if (typeof dependenciesJSON === 'object') {
      let result: Array<NpmDependency> = []
      for (const dependencyKey of Object.keys(dependenciesJSON)) {
        const dependencyValue = dependenciesJSON[dependencyKey]
        if (typeof dependencyValue === 'string') {
          result.push(npmDependency(dependencyKey, dependencyValue))
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
  npmDependencies: Array<NpmDependency>
}

let cachedDependencies: PackageJsonAndDeps | null = null

export function dependenciesFromPackageJson(
  packageJsonFile: ProjectFile | null,
): Array<NpmDependency> {
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
const EditorTypePackageDependencies: Array<NpmDependency> = [
  npmDependency('@types/react', '16.9.46'),
  npmDependency('@types/react-dom', '16.9.8'),
]

export function dependenciesWithEditorRequirements(
  projectContents: ProjectContents,
): Array<NpmDependency> {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const userDefinedDeps = dependenciesFromPackageJson(packageJsonFile)
  return [...userDefinedDeps, ...EditorTypePackageDependencies]
}

export function usePackageDependencies(): Array<NpmDependency> {
  const packageJsonFile = useEditorState((store) => {
    return packageJsonFileFromProjectContents(store.editor.projectContents)
  })

  return React.useMemo(() => {
    if (isCodeFile(packageJsonFile)) {
      return dependenciesFromPackageJsonContents(packageJsonFile.fileContents)
    } else {
      return []
    }
  }, [packageJsonFile])
}

export function useResolvedPackageDependencies(): Array<PossiblyUnversionedNpmDependency> {
  const basePackageDependencies = usePackageDependencies()
  const files = useEditorState((store) => {
    return store.editor.nodeModules.files
  })
  return React.useMemo(() => {
    return resolvedDependencyVersions(basePackageDependencies, files)
  }, [basePackageDependencies, files])
}

export function updateDependenciesInPackageJson(
  packageJson: string,
  npmDependencies: Array<NpmDependency>,
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
  npmDependencies: Array<NpmDependency>,
): EditorState {
  const transformPackageJson = (packageJson: string) => {
    return updateDependenciesInPackageJson(packageJson, npmDependencies)
  }
  return updatePackageJsonInEditorState(editor, transformPackageJson)
}

export function importResultFromImports(
  importOrigin: string,
  imports: Imports,
  requireFn: (origin: string, toImport: string) => any,
): MapLike<any> {
  let result: MapLike<any> = {}
  Utils.fastForEach(Object.keys(imports), (importSource) => {
    const importContent = imports[importSource]
    const requireResult = requireFn(importOrigin, importSource)
    if (requireResult == null) {
      console.warn(`Could not find ${importSource} with a require call.`)
    } else {
      if (importContent.importedWithName !== null) {
        result[importContent.importedWithName] = requireResult.default
      }
      if (importContent.importedAs !== null) {
        result[importContent.importedAs] = requireResult
      }
      Utils.fastForEach(importContent.importedFromWithin, (fromWithin) => {
        result[fromWithin.alias] = requireResult[fromWithin.name]
      })
    }
  })
  return result
}

export function createLoadedPackageStatusMapFromDependencies(
  dependencies: Array<NpmDependency>,
  dependenciesWithErrors: Array<NpmDependency>,
  dependenciesNotFound: Array<NpmDependency>,
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
