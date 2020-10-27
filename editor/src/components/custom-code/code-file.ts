import Utils from '../../utils/utils'
import {
  ExportType,
  ExportsInfo,
  MultiFileBuildResult,
  BuildType,
  EmitFileResult,
} from '../../core/workers/ts/ts-worker'
import { PropertyControls } from 'utopia-api'
import { RawSourceMap } from '../../core/workers/ts/ts-typings/RawSourceMap'
import { SafeFunction } from '../../core/shared/code-exec-utils'
import {
  getControlsForExternalDependencies,
  NodeModulesUpdate,
  sendPropertyControlsInfoRequest,
} from '../../core/property-controls/property-controls-utils'
import {
  NodeModules,
  esCodeFile,
  ProjectContents,
  isEsCodeFile,
} from '../../core/shared/project-file-types'

import { EditorDispatch } from '../editor/action-types'
import { getEditorRequireFn } from '../../core/es-modules/package-manager/package-manager'
import { updateNodeModulesContents } from '../editor/actions/actions'
import { fastForEach } from '../../core/shared/utils'
import { arrayToObject } from '../../core/shared/array-utils'
import { objectMap } from '../../core/shared/object-utils'
import { ProjectContentTreeRoot } from '../assets'
export interface CodeResult {
  exports: ModuleExportTypes
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
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
  error: Error | null
  requireFn: UtopiaRequireFn
  projectModules: MultiFileBuildResult
}

type ModuleExportValues = { [name: string]: any }
type ModuleExportTypes = { [name: string]: ExportType }
type ExportValue = { value: any }
type ModuleExportTypesAndValues = { [name: string]: ExportType & ExportValue }

export function getExportValuesFromAllModules(
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

export function processExportsInfo(
  exportValues: ModuleExportValues,
  exportTypes: ModuleExportTypes,
): {
  exports: ModuleExportTypesAndValues
  error: Error | null
} {
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

export function incorporateBuildResult(
  nodeModules: NodeModules,
  buildResult: MultiFileBuildResult,
): void {
  // Mutates nodeModules.
  fastForEach(Object.keys(buildResult), (moduleKey) => {
    const modulesFile = buildResult[moduleKey]
    if (modulesFile.transpiledCode != null) {
      nodeModules[moduleKey] = esCodeFile(modulesFile.transpiledCode, null)
    }
  })
}

export function clearNodeModules(nodeModules: NodeModules): NodeModules {
  return objectMap((nodeModule) => {
    if (isEsCodeFile(nodeModule)) {
      return esCodeFile(nodeModule.fileContents, null)
    } else {
      return nodeModule
    }
  }, nodeModules)
}

export function generateCodeResultCache(
  projectContents: ProjectContentTreeRoot,
  existingModules: MultiFileBuildResult,
  updatedModules: MultiFileBuildResult,
  exportsInfo: ReadonlyArray<ExportsInfo>,
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
  buildType: BuildType,
  mainUiFileName: string | null,
  onlyProjectFiles: boolean,
): CodeResultCache {
  // Makes the assumption that `fullBuild` and `updatedModules` are in line
  // with each other.
  let projectModules: MultiFileBuildResult
  switch (buildType) {
    case 'full-build':
      projectModules = { ...updatedModules }
      break
    case 'incremental':
      projectModules = {
        ...existingModules,
        ...updatedModules,
      }
      break
    default:
      const _exhaustiveCheck: never = buildType
      throw new Error(`Unhandled type ${JSON.stringify(buildType)}`)
  }

  // FIXME Rip this awful hack out after we tackle the dependency graph work!
  // Sneaky hack - if the currently edited file is a canvas file, we don't re-evaluate any other files
  const updatedFileNames = Object.keys(updatedModules)
  const onlyCanvasFileUpdated =
    buildType === 'incremental' &&
    mainUiFileName != null &&
    updatedFileNames.length === 1 &&
    (updatedFileNames[0] === mainUiFileName || updatedFileNames[0] === `/${mainUiFileName}`)

  if (!onlyCanvasFileUpdated) {
    // MUTATION ALERT! This function is mutating editorState.nodeModules.files by inserting the project files into it
    // FIXME Remove this mutation with the dependency graph work and store the eval cache for project files elsewhere
    // (maybe even in the graph itself)
    incorporateBuildResult(nodeModules, projectModules)
  }

  // Trigger async call to build the property controls info.
  sendPropertyControlsInfoRequest(exportsInfo, nodeModules, projectContents, onlyProjectFiles)

  const requireFn = getEditorRequireFn(projectContents, nodeModules, dispatch)

  let cache: { [code: string]: CodeResult } = {}
  Utils.fastForEach(exportsInfo, (result) => {
    cache[result.filename] = {
      exports: result.exportTypes,
      ...projectModules[result.filename],
    }
  })

  return {
    skipDeepFreeze: true,
    exportsInfo: exportsInfo,
    cache: cache,
    error: null,
    requireFn: requireFn,
    projectModules: projectModules,
  }
}

export function isJavascriptOrTypescript(filePath: string): boolean {
  const regex = /\.(js|jsx|ts|tsx)$/
  return regex.test(filePath)
}

export function codeCacheToBuildResult(cache: {
  [filename: string]: CodeResult
}): { [filename: string]: EmitFileResult } {
  return objectMap((entry) => {
    return {
      transpiledCode: entry.transpiledCode,
      sourceMap: entry.sourceMap,
      errors: [], // TODO: this is ugly, these errors are the build errors which are not stored in CodeResultCache, but directly in EditorState.codeEditorErrors
    }
  }, cache)
}
