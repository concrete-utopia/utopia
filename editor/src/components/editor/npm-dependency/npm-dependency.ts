import * as json5 from 'json5'
import * as R from 'ramda'
import * as ImportedReact from 'react'
import * as ImportedReactDOM from 'react-dom'
import { MapLike } from 'typescript'
import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from 'uuiui'
import * as UUIUIDeps from 'uuiui-deps'
import { PACKAGER_ENTRY_POINT, UTOPIA_BACKEND } from '../../../common/env-vars'
import { sameCodeFile } from '../../../core/model/project-file-utils'
import {
  NpmBundleResult,
  NpmDependencies,
  RequireFn,
  TypeDefinitions,
  NpmDependency,
  PackagerServerResponse,
  TypingsServerResponse,
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
  packageJsonFileFromModel,
  updatePackageJsonInEditorState,
} from '../store/editor-state'
import { reactDomTypings, reactGlobalTypings, reactTypings } from './react-typings'
import { utopiaApiTypings } from './utopia-api-typings'
import { objectMap } from '../../../core/shared/object-utils'
import { mapArrayToDictionary } from '../../../core/shared/array-utils'

export async function findLatestVersion(packageName: string): Promise<string> {
  const requestInit: RequestInit = {
    headers: {
      accept: 'application/vnd.npm.install-v1+json; q=1.0, application/json; q=0.8, */*',
    },
  }

  // Calls out to our services because of the wonder of CORS.
  const response = await fetch(
    `${UTOPIA_BACKEND}javascript/package/metadata/${encodeURIComponent(packageName).replace(
      /^%40/,
      '@',
    )}`,
    requestInit,
  )
  if (response.ok) {
    const packageJson = await response.json()
    const latestVersion = Utils.path(['dist-tags', 'latest'], packageJson)
    if (latestVersion == null || typeof latestVersion != 'string') {
      return Promise.reject(`Received invalid content for package ${packageName}`)
    } else {
      return Promise.resolve(latestVersion)
    }
  } else {
    return Promise.reject(`Received an error for package ${packageName}`)
  }
}

export function dependenciesFromPackageJsonContents(packageJson: string): NpmDependencies | null {
  try {
    const parsedJSON = json5.parse(packageJson)

    const dependenciesJSON = R.path<any>(['dependencies'], parsedJSON)
    if (typeof dependenciesJSON === 'object') {
      let result: NpmDependencies = {}
      for (const dependencyKey of Object.keys(dependenciesJSON)) {
        const dependencyValue = dependenciesJSON[dependencyKey]
        if (typeof dependencyValue === 'string') {
          result[dependencyKey] = dependencyValue
        } else {
          return null
        }
      }
      return result
    } else {
      return null
    }
  } catch (error) {
    return null
  }
}

// Cache the dependencies when getting them from `EditorState` because lots of things need this information.
// IMPORTANT: This caching is relied upon indirectly by monaco-wrapper.tsx
interface PackageJsonAndDeps {
  packageJsonFile: ProjectFile
  npmDependencies: NpmDependencies
}

let cachedDependencies: PackageJsonAndDeps | null = null

export function dependenciesFromPackageJson(
  packageJsonFile: ProjectFile | null,
): NpmDependencies | null {
  if (packageJsonFile == null) {
    return {}
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

export function dependenciesFromModel(model: {
  projectContents: ProjectContents
}): NpmDependencies | null {
  const packageJsonFile = packageJsonFileFromModel(model)
  return dependenciesFromPackageJson(packageJsonFile)
}

export function updateDependenciesInPackageJson(
  packageJson: string,
  npmDependencies: NpmDependencies,
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
  npmDependencies: NpmDependencies,
): EditorState {
  const transformPackageJson = (packageJson: string) => {
    return updateDependenciesInPackageJson(packageJson, npmDependencies)
  }
  return updatePackageJsonInEditorState(editor, transformPackageJson)
}

export function importResultFromImports(
  importOrigin: string,
  imports: Imports,
  requireFn: (importOrigin: string, toImport: string) => any,
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
