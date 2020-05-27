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
} from '../../../core/shared/npm-dependency-types'
import {
  isCodeFile,
  Imports,
  ProjectContents,
  ProjectFile,
} from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import { normalizeName } from '../../custom-code/custom-code-utils'
import { EditorDispatch } from '../action-types'
import { updateNpmDependencies } from '../actions/actions'
import {
  EditorState,
  packageJsonFileFromModel,
  updatePackageJsonInEditorState,
} from '../store/editor-state'
import { reactDomTypings, reactGlobalTypings, reactTypings } from './react-typings'
import { utopiaApiTypings } from './utopia-api-typings'

interface Manifest {
  externals: { [key: string]: string }
}

type DLL = string

let manifestCache: { [key: string]: Manifest } = {} // { [key: URL]: Manifest }
let dllCache: { [key: string]: DLL } = {} // { [key: URL]: DLL }

async function fetchManifest(deps: string): Promise<Manifest> {
  const url = `${PACKAGER_ENTRY_POINT}${deps}/manifest.json`
  if (manifestCache[url] != null) {
    return Promise.resolve(manifestCache[url])
  } else {
    const response = await fetch(`${PACKAGER_ENTRY_POINT}${deps}/manifest.json`)
    if (response.ok) {
      const manifest = await response.json()
      // TODO investigate if eslint is right witht this error
      // eslint-disable-next-line require-atomic-updates
      manifestCache[url] = manifest
      return manifest
    } else {
      console.error(
        `Invalid manifest returned for dependencies ${deps}: ${response.status} - ${response.statusText}`,
      )
      throw new Error(`Invalid manifest returned for dependencies ${deps}`)
    }
  }
}

async function fetchDll(deps: string): Promise<DLL> {
  const url = `${PACKAGER_ENTRY_POINT}${deps}/dll.js`
  if (dllCache[url] != null) {
    return Promise.resolve(dllCache[url])
  } else {
    const response = await fetch(`${PACKAGER_ENTRY_POINT}${deps}/dll.js`)
    if (response.ok) {
      const dll = await response.text()
      // TODO investigate if eslint is right witht this error
      // eslint-disable-next-line require-atomic-updates
      dllCache[url] = dll
      return dll
    } else {
      console.error(
        `Invalid bundle returned for dependencies ${deps}: ${response.status} - ${response.statusText}`,
      )
      throw new Error(`Invalid bundle returned for dependencies ${deps}`)
    }
  }
}

/* eslint-disable @typescript-eslint/no-unused-vars */

const createRequire = (manifest: Manifest, dll: DLL, replaceReact: boolean) => {
  // please keep the stupid peer dependencies in sync. search for this comment!
  const React = ImportedReact
  const ReactDOM = ImportedReactDOM

  /**
   * appending 'dll_bundle' to the bottom of the dll file makes `eval()` return
   * the __webpack_require__() function. This somewhat depends on the
   * configuration of the packager server we use.
   */
  const dllReturningWebpackRequire = dll + 'dll_bundle'
  const requireFn = eval(dllReturningWebpackRequire)
  return (importOrigin: string, toImport: string) => {
    const normalizedName = normalizeName(importOrigin, toImport)
    /**
     * DO NOT RELEASE THE SOFTWARE WITH THIS ENABLED
     * OR AT LEAST WITHOUT REVISITING THIS TOPIC
     * THIS IS FOR MAKING LOCAL DEVELOPMENT EASIER FOR THE MONTH OF AUGUST
     *
     * we are returning UtopiaAPI from the editor bundle, instead of the npm package bundle returned from the server here.
     * why? because this enables us to skip the bundler server and the bumping procedure while iterating on the utopia-api
     *
     * once the API is stable, we need to start versioning it, and then this hack will need to be removed
     * and importResultFromImports should be updated too
     */
    if (normalizedName === 'utopia-api') {
      return {
        ...UtopiaAPI,
        default: UtopiaAPI,
      }
    }

    if (normalizedName === 'uuiui') {
      return {
        ...UUIUI,
        default: UUIUI,
      }
    }

    if (normalizedName === 'uuiui-deps') {
      return {
        ...UUIUIDeps,
        default: UUIUIDeps,
      }
    }

    if (replaceReact && normalizedName === 'react') {
      return {
        ...React,
        default: React,
      }
    }

    if (replaceReact && normalizedName === 'react-dom') {
      return {
        ...ReactDOM,
        default: ReactDOM,
      }
    }

    /**
     * manifest.externals will contain the module index buried in a string like 'dll_bundle(15)'
     * but we are only interested in the index.
     */
    const webpackModuleIndex = /dll_bundle\(([0-9]*)\)/.exec(manifest.externals[normalizedName])
    if (webpackModuleIndex == null) {
      return undefined
    }
    const toEval = webpackModuleIndex[1]
    try {
      const requireResult = requireFn(toEval)
      if (requireResult === undefined) {
        // this is here so our "fail on imports" hack doesn't detect a false positive in es6-micro-loader
        return null
      }
      return requireResult
    } catch (e) {
      console.error(
        `ERROR in require(${normalizedName}) when evaluating __webpack_require__(${toEval}):`,
        e,
      )
      throw e
    }
  }
}

function collectTypeDefinitions(manifest: Manifest, require: RequireFn): TypeDefinitions {
  const fileList = Object.keys(manifest.externals)
  let typeDefinitions: TypeDefinitions = {}
  fileList
    .filter((file) => file.endsWith('.d.ts') || file.endsWith('package.json'))
    .forEach((filename) => {
      const definition = require('/', filename)
      if (filename.endsWith('.d.ts')) {
        typeDefinitions[filename] = definition
      } else if (filename.endsWith('package.json')) {
        typeDefinitions[filename] = JSON.stringify(definition)
      }
    })

  // please keep the stupid peer dependencies in sync. search for this comment!
  typeDefinitions['react/index.d.ts'] = reactTypings
  typeDefinitions['react/global.d.ts'] = reactGlobalTypings
  typeDefinitions['react-dom/index.d.ts'] = reactDomTypings
  typeDefinitions['utopia-api/index.d.ts'] = utopiaApiTypings

  return typeDefinitions
}

const emptyBundleResult: NpmBundleResult = {
  require: createRequire({ externals: {} }, 'const dll_bundle = () => {};', true), // we provide an empty require function (React and utopia-api will still work)
  typeDefinitions: {},
}

// The below function is called a lot when the results don't change, so cache the last result only
let lastBundle: { deps: string; bundle: NpmBundleResult } = { deps: '', bundle: emptyBundleResult }

export async function bundleNpmPackages(
  packages: NpmDependencies,
  replaceReact: boolean,
): Promise<NpmBundleResult> {
  const depNames = Object.keys(packages)
  if (depNames.length === 0) {
    return emptyBundleResult
  }
  const filtered = replaceReact
    ? depNames.filter((name) => name != 'react' && name != 'react-dom' && name != 'utopia-api')
    : depNames
  const depStrings = filtered.map((name) => `${name}@${packages[name]}`)
  const deps = depStrings.join('+')
  if (lastBundle.deps === deps) {
    return Promise.resolve(lastBundle.bundle)
  }

  const manifest = await fetchManifest(deps)
  const dll = await fetchDll(deps)
  const require = createRequire(manifest, dll, replaceReact)
  const typeDefinitions = collectTypeDefinitions(manifest, require)
  const result = {
    require: require,
    typeDefinitions: typeDefinitions,
  }
  // TODO investigate if eslint is right witht this error
  // https://eslint.org/docs/rules/require-atomic-updates#disallow-assignments-that-can-lead-to-race-conditions-due-to-usage-of-await-or-yield-require-atomic-updates
  // eslint-disable-next-line require-atomic-updates
  lastBundle.deps = deps
  // eslint-disable-next-line require-atomic-updates
  lastBundle.bundle = result
  return result
}

// TODO add react and react-dom typings no matter what

export async function bundleAndDispatchNpmPackages(
  dispatch: EditorDispatch,
  packages: NpmDependencies,
): Promise<void> {
  const bundleResult = await bundleNpmPackages(packages, true)
  dispatch(
    [updateNpmDependencies(packages, bundleResult.require, bundleResult.typeDefinitions)],
    'everyone',
  )
}

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
