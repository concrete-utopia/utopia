import Utils from '../../utils/utils'
import * as Es6MicroLoader from './es6-micro-loader'
import { RequireFn, NpmDependency } from '../../core/shared/npm-dependency-types'
import { ExportType, ExportsInfo, MultiFileBuildResult } from '../../core/workers/ts/ts-worker'
import { PropertyControls } from 'utopia-api'
import { RawSourceMap } from '../../core/workers/ts/ts-typings/RawSourceMap'
import { SafeFunction } from '../../core/shared/code-exec-utils'
import { getControlsForExternalDependencies } from '../../core/property-controls/property-controls-utils'
import { NodeModules } from '../../core/shared/project-file-types'

import { EditorDispatch } from '../editor/action-types'
import { getMemoizedRequireFn } from '../../core/es-modules/package-manager/package-manager'
import { updateNodeModulesContents } from '../editor/actions/actions'
export interface CodeResult {
  exports: ModuleExportTypesAndValues
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
  error: Error | null
}

// UtopiaRequireFn is a special require function, where you can control whether the evaluation of the code should happen only once or more.
// Standard JS behavior is to evaluate modules once lazily (the first time an import is processed), then cache
// the value of the exports, and then use these values later. However, in our system this is not the desired behavior, because we need to evaluate the imports
// in the spy, and then we need them for canvas rendering too. During canvas rendering we would like to see the exceptions coming from the evaluation,
// even though that is not the first import. So we need to be able to run the require function in a way that it does not have the side effect
// to cache/register the exported values from the module. When `skipRegistering` is `true`, then the exported values are not registered,
// and another call of the require function will evaluate the module again.
export type UtopiaRequireFn = (
  importOrigin: string,
  toImport: string,
  skipRegistering: boolean,
) => any

export type PropertyControlsInfo = {
  [filenameNoExtension: string]: { [componentName: string]: PropertyControls }
}

export type CodeResultCache = {
  skipDeepFreeze: true
  cache: { [filename: string]: CodeResult }
  exportsInfo: ReadonlyArray<ExportsInfo>
  propertyControlsInfo: PropertyControlsInfo
  error: Error | null
  requireFn: UtopiaRequireFn
}

type ModuleExportValues = { [name: string]: any }
type ModuleExportTypes = { [name: string]: ExportType }
type ExportValue = { value: any }
type ModuleExportTypesAndValues = { [name: string]: ExportType & ExportValue }

export function processModuleCodes(
  buildResult: MultiFileBuildResult,
  npmRequireFn: RequireFn,
  fullBuild: boolean,
): {
  requireFn: UtopiaRequireFn
  error: Error | null
} {
  const System = Es6MicroLoader.System
  Es6MicroLoader.reset(npmRequireFn, fullBuild)

  let error: Error | null = null

  const moduleNames = Object.keys(buildResult)

  // First eval all buildResult to fill in the System object with the transpiled codes
  moduleNames.forEach((moduleName) => {
    const module = buildResult[moduleName]
    if (module.transpiledCode == null) {
      return
    }
    try {
      // the transpiled code contains calls to System.register
      // this is the point where we populate Es6microLoader with the project files
      System.setModule(moduleName, module.sourceMap)
      eval(module.transpiledCode)
    } catch (e) {
      // skipping this module, there is a runtime error evaluating it
      // we only store the last error, that is enough now
      error = e
    }
  })

  // This requireFn uses the System created and populated above, containing all project files + the npm bundle
  const requireFn = (importOrigin: string, toImport: string, silent: boolean) => {
    return System.getModule(importOrigin, toImport, silent)
  }

  return {
    requireFn: requireFn,
    error: error,
  }
}

function getExportValuesFromAllModules(
  buildResult: MultiFileBuildResult,
  requireFn: UtopiaRequireFn,
): { [module: string]: ModuleExportValues } {
  /**
   * TODO
   * we are requiring every user module here. unfortunately it means that if
   * requiring them has any side effect, we will trigger that side effect here,
   * even if it was never imported by the user
   *
   * a better solution would be to store the exported values as a side effect of the user requiring the module
   * that way the side effects would happen at the correct time, and we would
   * still have access to things like the PropertyControls for every component the user
   * can select (since selecting them requires the component to be on screen, which means it must be imported anyways)
   *
   */

  let exports: { [module: string]: ModuleExportValues } = {}
  const moduleNames = Object.keys(buildResult)
  // get all the modules from System to fill in the exports with their values
  moduleNames.forEach((moduleName) => {
    if (moduleName.toLowerCase().endsWith('.css')) {
      // Skip eager evalution of css
      return
    }

    const module = buildResult[moduleName]
    if (module.transpiledCode == null) {
      return
    }
    try {
      exports[moduleName] = {}
      const codeModule = requireFn('/', moduleName, true)
      if (codeModule != null) {
        Object.keys(codeModule).forEach((exp) => {
          exports[moduleName][exp] = codeModule[exp]
        })
      }
    } catch (e) {
      // skipping this module, there is a runtime error executing it
    }
  })
  return exports
}

function processExportsInfo(exportValues: ModuleExportValues, exportTypes: ModuleExportTypes) {
  let exportsWithType: ModuleExportTypesAndValues = {}
  try {
    Utils.fastForEach(Object.keys(exportValues), (name: string) => {
      if (exportTypes[name] == null) {
        exportsWithType[name] = {
          value: exportValues[name],
          type: 'any',
          functionInfo: null,
          reactClassInfo: null,
        }
      } else {
        exportsWithType[name] = {
          ...exportTypes[name],
          value: exportValues[name],
        }
      }
    })

    return {
      exports: exportsWithType,
      error: null,
    }
  } catch (e) {
    return {
      exports: exportsWithType,
      error: e,
    }
  }
}

export function generateCodeResultCache(
  modules: MultiFileBuildResult,
  exportsInfo: ReadonlyArray<ExportsInfo>,
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
  npmDependencies: NpmDependency[],
  fullBuild: boolean,
): CodeResultCache {
  const npmRequireFn = getMemoizedRequireFn(nodeModules, dispatch)

  const { requireFn, error } = processModuleCodes(modules, npmRequireFn, fullBuild)
  const exportValues = getExportValuesFromAllModules(modules, requireFn)
  let cache: { [code: string]: CodeResult } = {}
  let propertyControlsInfo: PropertyControlsInfo = getControlsForExternalDependencies(
    npmDependencies,
  )
  Utils.fastForEach(exportsInfo, (result) => {
    const codeResult = processExportsInfo(exportValues[result.filename], result.exportTypes)
    cache[result.filename] = {
      ...codeResult,
      ...modules[result.filename],
    }
    let propertyControls: { [name: string]: PropertyControls } = {}
    if (codeResult.exports != null) {
      Utils.fastForEach(Object.keys(codeResult.exports), (name) => {
        const exportedObject = codeResult.exports[name].value
        if (exportedObject != null && exportedObject.propertyControls != null) {
          // FIXME validate shape
          propertyControls[name] = exportedObject.propertyControls
        }
      })
      const filenameNoExtension = result.filename.replace(/\.(js|jsx|ts|tsx)$/, '')
      propertyControlsInfo[filenameNoExtension] = propertyControls
    }
  })

  return {
    skipDeepFreeze: true,
    exportsInfo: exportsInfo,
    cache: cache,
    propertyControlsInfo: propertyControlsInfo,
    error: error,
    requireFn: requireFn,
  }
}

export function isJavascriptOrTypescript(filePath: string): boolean {
  const regex = /\.(js|jsx|ts|tsx)$/
  return regex.test(filePath)
}

export const codeCacheToBuildResult = (cache: { [filename: string]: CodeResult }) => {
  const multiFileBuildResult = Object.keys(cache).reduce((acc, filename) => {
    return {
      ...acc,
      [filename]: {
        transpiledCode: cache[filename].transpiledCode,
        sourceMap: cache[filename].sourceMap,
        errors: [], // TODO: this is ugly, these errors are the build errors which are not stored in CodeResultCache, but directly in EditorState.codeEditorErrors
      },
    }
  }, {} as MultiFileBuildResult)

  return multiFileBuildResult
}
