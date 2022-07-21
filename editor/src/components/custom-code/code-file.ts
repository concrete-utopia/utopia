import Utils from '../../utils/utils'
import { EmitFileResult } from '../../core/workers/ts/ts-worker'
import { RawSourceMap } from '../../core/workers/ts/ts-typings/RawSourceMap'
import {
  NodeModules,
  esCodeFile,
  ProjectContents,
  isEsCodeFile,
  ElementPath,
  TextFile,
  isTextFile,
  RevisionsState,
  isParseSuccess,
  StaticElementPath,
  ParseSuccess,
  Imports,
} from '../../core/shared/project-file-types'

import { EditorDispatch } from '../editor/action-types'
import {
  EvaluationCache,
  getCurriedEditorRequireFn,
} from '../../core/es-modules/package-manager/package-manager'
import { fastForEach } from '../../core/shared/utils'
import { arrayToObject } from '../../core/shared/array-utils'
import { objectMap } from '../../core/shared/object-utils'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'
import { Either, isRight, left, right } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import {
  getJSXAttribute,
  isIntrinsicElement,
  isJSXAttributeOtherJavaScript,
  isUtopiaJSXComponent,
  JSXElement,
  JSXElementWithoutUID,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import { findElementWithUID } from '../../core/shared/uid-utils'
import { importedFromWhere } from '../editor/import-utils'
import {
  resolveModule,
  resolveModulePath,
} from '../../core/es-modules/package-manager/module-resolution'
import { getTransitiveReverseDependencies } from '../../core/shared/project-contents-dependencies'
import { optionalMap } from '../../core/shared/optional-utils'
import { findJSXElementAtStaticPath } from '../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../core/model/project-file-utils'
import {
  ExportsInfo,
  MultiFileBuildResult,
  ExportType,
  BuildType,
} from '../../core/workers/common/worker-types'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { ParsedPropertyControls } from '../../core/property-controls/property-controls-parser'
import { ParseResult } from '../../utils/value-parser-utils'
import { PropertyControls } from 'utopia-api/core'

type ModuleExportTypes = { [name: string]: ExportType }

export interface CodeResult {
  exports: ModuleExportTypes
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
}

export function codeResult(
  exports: ModuleExportTypes,
  transpiledCode: string | null,
  sourceMap: RawSourceMap | null,
): CodeResult {
  return {
    exports: exports,
    transpiledCode: transpiledCode,
    sourceMap: sourceMap,
  }
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

export type CurriedUtopiaRequireFn = (projectContents: ProjectContentTreeRoot) => UtopiaRequireFn

export interface ComponentInfo {
  insertMenuLabel: string
  elementToInsert: JSXElementWithoutUID
  importsToAdd: Imports
}

export function componentInfo(
  insertMenuLabel: string,
  elementToInsert: JSXElementWithoutUID,
  importsToAdd: Imports,
): ComponentInfo {
  return {
    insertMenuLabel: insertMenuLabel,
    elementToInsert: elementToInsert,
    importsToAdd: importsToAdd,
  }
}

export interface ComponentDescriptor {
  properties: PropertyControls
  variants: ComponentInfo[]
}

export function componentDescriptor(
  properties: PropertyControls,
  variants: Array<ComponentInfo>,
): ComponentDescriptor {
  return {
    properties: properties,
    variants: variants,
  }
}

export interface ComponentDescriptorWithName extends ComponentDescriptor {
  componentName: string
}

export type ComponentDescriptorsForFile = {
  [componentName: string]: ComponentDescriptor
}

export type PropertyControlsInfo = {
  [filenameNoExtension: string]: ComponentDescriptorsForFile
}

export type ResolveFn = (importOrigin: string, toImport: string) => Either<string, string>
export type CurriedResolveFn = (projectContents: ProjectContentTreeRoot) => ResolveFn

export interface CodeResultCache {
  skipDeepFreeze: true
  cache: { [filename: string]: CodeResult }
  exportsInfo: Array<ExportsInfo>
  error: Error | null
  curriedRequireFn: CurriedUtopiaRequireFn
  curriedResolveFn: CurriedResolveFn
  projectModules: MultiFileBuildResult
  evaluationCache: EvaluationCache
}

export function codeResultCache(
  cache: { [filename: string]: CodeResult },
  exportsInfo: Array<ExportsInfo>,
  error: Error | null,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
  projectModules: MultiFileBuildResult,
  evaluationCache: EvaluationCache,
): CodeResultCache {
  return {
    skipDeepFreeze: true,
    cache: cache,
    exportsInfo: exportsInfo,
    error: error,
    curriedRequireFn: curriedRequireFn,
    curriedResolveFn: curriedResolveFn,
    projectModules: projectModules,
    evaluationCache: evaluationCache,
  }
}

export function incorporateBuildResult(
  nodeModules: NodeModules,
  projectContents: ProjectContentTreeRoot,
  buildResult: MultiFileBuildResult,
): void {
  // Mutates nodeModules.
  fastForEach(Object.keys(buildResult), (moduleKey) => {
    const modulesFile = buildResult[moduleKey]
    if (modulesFile.transpiledCode == null) {
      delete nodeModules[moduleKey]
    } else {
      const projectContentsFile = getContentsTreeFileFromString(projectContents, moduleKey)
      const origin = projectContentsFile == null ? 'NODE_MODULES' : 'PROJECT_CONTENTS'
      nodeModules[moduleKey] = esCodeFile(modulesFile.transpiledCode, origin, moduleKey)
    }
  })
  // Cleanup non-existant project files.
  fastForEach(Object.keys(nodeModules), (moduleKey) => {
    const modulesFile = nodeModules[moduleKey]
    if (isEsCodeFile(modulesFile)) {
      if (modulesFile.origin === 'PROJECT_CONTENTS') {
        const projectContentsFile = getContentsTreeFileFromString(projectContents, moduleKey)
        if (projectContentsFile == null) {
          delete nodeModules[moduleKey]
        }
      }
    }
  })
}

const getCurriedEditorResolveFunction =
  (nodeModules: NodeModules): CurriedResolveFn =>
  (projectContents: ProjectContentTreeRoot) =>
  (importOrigin: string, toImport: string) =>
    resolveModulePath(projectContents, nodeModules, importOrigin, toImport)

export function generateCodeResultCache(
  projectContents: ProjectContentTreeRoot,
  updatedModules: MultiFileBuildResult,
  exportsInfo: Array<ExportsInfo>,
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
  evaluationCache: EvaluationCache,
  builtInDependencies: BuiltInDependencies,
): CodeResultCache {
  const projectModules = { ...updatedModules }
  const updatedFileNames = Object.keys(updatedModules)
  const updatedAndReverseDepFilenames = getTransitiveReverseDependencies(
    projectContents,
    nodeModules,
    updatedFileNames,
  )

  // Mutating the evaluation cache.
  for (const fileToDelete of updatedAndReverseDepFilenames) {
    delete evaluationCache[fileToDelete]
  }

  // MUTATION ALERT! This function is mutating editorState.nodeModules.files by inserting the project files into it.
  incorporateBuildResult(nodeModules, projectContents, projectModules)

  const curriedRequireFn = getCurriedEditorRequireFn(
    nodeModules,
    dispatch,
    evaluationCache,
    builtInDependencies,
  )
  const curriedResolveFn = getCurriedEditorResolveFunction(nodeModules)

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
    curriedRequireFn: curriedRequireFn,
    curriedResolveFn: curriedResolveFn,
    projectModules: projectModules,
    evaluationCache: evaluationCache,
  }
}

export function isJavascriptOrTypescript(filePath: string): boolean {
  const regex = /\.(js|jsx|ts|tsx)$/
  return regex.test(filePath)
}

export function codeCacheToBuildResult(cache: { [filename: string]: CodeResult }): {
  [filename: string]: EmitFileResult
} {
  return objectMap((entry) => {
    return {
      transpiledCode: entry.transpiledCode,
      sourceMap: entry.sourceMap,
      errors: [], // TODO: this is ugly, these errors are the build errors which are not stored in CodeResultCache, but directly in EditorState.codeEditorErrors
    }
  }, cache)
}

export interface NormalisePathSuccess {
  type: 'NORMALISE_PATH_SUCCESS'
  normalisedPath: StaticElementPath | null
  filePath: string
  textFile: TextFile
  normalisedDynamicPath: ElementPath | null
}

export function normalisePathSuccess(
  normalisedPath: StaticElementPath | null,
  filePath: string,
  textFile: TextFile,
  normalisedDynamicPath: ElementPath | null,
): NormalisePathSuccess {
  return {
    type: 'NORMALISE_PATH_SUCCESS',
    normalisedPath: normalisedPath,
    filePath: filePath,
    textFile: textFile,
    normalisedDynamicPath: normalisedDynamicPath,
  }
}

export interface NormalisePathEndsAtDependency {
  type: 'NORMALISE_PATH_ENDS_AT_DEPENDENCY'
  dependency: string
}

export function normalisePathEndsAtDependency(dependency: string): NormalisePathEndsAtDependency {
  return {
    type: 'NORMALISE_PATH_ENDS_AT_DEPENDENCY',
    dependency: dependency,
  }
}

export interface NormalisePathEndsAtIgnoredDependency {
  type: 'NORMALISE_PATH_ENDS_AT_IGNORED_DEPENDENCY'
  dependency: string
}

export function normalisePathEndsAtIgnoredDependency(
  dependency: string,
): NormalisePathEndsAtIgnoredDependency {
  return {
    type: 'NORMALISE_PATH_ENDS_AT_IGNORED_DEPENDENCY',
    dependency: dependency,
  }
}

export interface NormalisePathError {
  type: 'NORMALISE_PATH_ERROR'
  errorMessage: string
}

export function normalisePathError(errorMessage: string): NormalisePathError {
  return {
    type: 'NORMALISE_PATH_ERROR',
    errorMessage: errorMessage,
  }
}

export interface NormalisePathUnableToProceed {
  type: 'NORMALISE_PATH_UNABLE_TO_PROCEED'
  filePath: string
}

export function normalisePathUnableToProceed(filePath: string): NormalisePathUnableToProceed {
  return {
    type: 'NORMALISE_PATH_UNABLE_TO_PROCEED',
    filePath: filePath,
  }
}

export interface NormalisePathImportNotFound {
  type: 'NORMALISE_PATH_IMPORT_NOT_FOUND'
  notFound: string
}

export function normalisePathImportNotFound(notFound: string): NormalisePathImportNotFound {
  return {
    type: 'NORMALISE_PATH_IMPORT_NOT_FOUND',
    notFound: notFound,
  }
}

export type NormalisePathResult =
  | NormalisePathError
  | NormalisePathUnableToProceed
  | NormalisePathImportNotFound
  | NormalisePathEndsAtDependency
  | NormalisePathEndsAtIgnoredDependency
  | NormalisePathSuccess

export function normalisePathSuccessOrThrowError(
  normalisePathResult: NormalisePathResult,
): NormalisePathSuccess {
  switch (normalisePathResult.type) {
    case 'NORMALISE_PATH_SUCCESS':
      return normalisePathResult
    case 'NORMALISE_PATH_ERROR':
      throw new Error(normalisePathResult.errorMessage)
    case 'NORMALISE_PATH_IMPORT_NOT_FOUND':
      throw new Error(`Could not find an import (${normalisePathResult.notFound}).`)
    case 'NORMALISE_PATH_UNABLE_TO_PROCEED':
      throw new Error(`Could not proceed past ${normalisePathResult.filePath}.`)
    case 'NORMALISE_PATH_ENDS_AT_DEPENDENCY':
      throw new Error(`Reached an external dependency ${normalisePathResult.dependency}.`)
    case 'NORMALISE_PATH_ENDS_AT_IGNORED_DEPENDENCY':
      throw new Error(
        `Reached an external dependency that was ignored by the package.json browser field ${normalisePathResult.dependency}.`,
      )
    default:
      const _exhaustiveCheck: never = normalisePathResult
      throw new Error(`Unhandled case ${JSON.stringify(normalisePathResult)}`)
  }
}

export function normalisePathToUnderlyingTarget(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  currentFilePath: string,
  elementPath: ElementPath | null,
): NormalisePathResult {
  const currentFile = getContentsTreeFileFromString(projectContents, currentFilePath)
  if (isTextFile(currentFile)) {
    if (isParseSuccess(currentFile.fileContents.parsed)) {
      const staticPath = elementPath == null ? null : EP.dynamicPathToStaticPath(elementPath)
      const potentiallyDroppedFirstPathElementResult = EP.dropFirstPathElement(elementPath)
      if (potentiallyDroppedFirstPathElementResult.droppedPathElements == null) {
        // As the scene path is empty, there's no more traversing to do, the target is in this file.
        return normalisePathSuccess(staticPath, currentFilePath, currentFile, elementPath)
      } else {
        const droppedPathPart = potentiallyDroppedFirstPathElementResult.droppedPathElements
        if (droppedPathPart.length === 0) {
          return normalisePathError(
            `Unable to handle empty scene path part for ${optionalMap(EP.toString, elementPath)}`,
          )
        } else {
          // Now need to identify the element relating to the last part of the dropped scene path.
          const lastDroppedPathPart = droppedPathPart[droppedPathPart.length - 1]

          // Walk the parsed representation to find the element with the given uid.
          const parsedContent = currentFile.fileContents.parsed
          let targetElement: JSXElement | null = null
          for (const topLevelElement of parsedContent.topLevelElements) {
            const possibleTarget = findElementWithUID(topLevelElement, lastDroppedPathPart)
            if (possibleTarget != null) {
              targetElement = possibleTarget
              break
            }
          }

          // Identify where the component is imported from or if it's in the same file.
          if (targetElement == null) {
            return normalisePathImportNotFound(lastDroppedPathPart)
          } else {
            const nonNullTargetElement: JSXElement = targetElement

            // Handle things like divs.
            if (isIntrinsicElement(targetElement.name)) {
              return normalisePathSuccess(
                potentiallyDroppedFirstPathElementResult.newPath == null
                  ? null
                  : EP.dynamicPathToStaticPath(potentiallyDroppedFirstPathElementResult.newPath),
                currentFilePath,
                currentFile,
                potentiallyDroppedFirstPathElementResult.newPath,
              )
            } else {
              return lookupElementImport(
                targetElement.name.baseVariable,
                currentFilePath,
                projectContents,
                nodeModules,
                nonNullTargetElement,
                elementPath,
                parsedContent,
                potentiallyDroppedFirstPathElementResult,
              )
            }
          }
        }
      }
    } else {
      return normalisePathUnableToProceed(currentFilePath)
    }
  } else {
    return normalisePathUnableToProceed(currentFilePath)
  }
}

function lookupElementImport(
  elementBaseVariable: string,
  currentFilePath: string,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  nonNullTargetElement: JSXElement,
  elementPath: ElementPath | null,
  parsedContent: ParseSuccess,
  potentiallyDroppedFirstPathElementResult: EP.DropFirstPathElementResultType,
): NormalisePathResult {
  const importedFrom = importedFromWhere(
    currentFilePath,
    elementBaseVariable,
    parsedContent.topLevelElements,
    parsedContent.imports,
  )
  if (importedFrom == null) {
    return normalisePathImportNotFound(elementBaseVariable)
  } else {
    if (
      importedFrom.type === 'IMPORTED_ORIGIN' &&
      importedFrom.filePath === 'utopia-api' &&
      importedFrom.exportedName === 'Scene'
    ) {
      // Navigate around the scene with the special case handling.
      const componentAttr = getJSXAttribute(nonNullTargetElement.props, 'component')
      if (componentAttr != null && isJSXAttributeOtherJavaScript(componentAttr)) {
        return lookupElementImport(
          componentAttr.javascript,
          currentFilePath,
          projectContents,
          nodeModules,
          nonNullTargetElement,
          elementPath,
          parsedContent,
          potentiallyDroppedFirstPathElementResult,
        )
      } else {
        return normalisePathError(
          `Unable to handle Scene component definition for ${optionalMap(
            EP.toString,
            elementPath,
          )}`,
        )
      }
    } else {
      const resolutionResult = resolveModule(
        projectContents,
        nodeModules,
        currentFilePath,
        importedFrom.filePath,
      )
      switch (resolutionResult.type) {
        case 'RESOLVE_SUCCESS':
          const successResult = resolutionResult.success
          // Avoid drilling into node_modules because we can't do anything useful with
          // the contents of files in there.
          if (successResult.path.startsWith('/node_modules/')) {
            const splitPath = successResult.path.split('/')
            return normalisePathEndsAtDependency(splitPath[2])
          } else {
            switch (successResult.file.type) {
              case 'ES_CODE_FILE':
                return normalisePathToUnderlyingTarget(
                  projectContents,
                  nodeModules,
                  successResult.path,
                  potentiallyDroppedFirstPathElementResult.newPath,
                )
              case 'ES_REMOTE_DEPENDENCY_PLACEHOLDER':
                return normalisePathUnableToProceed(successResult.path)
              default:
                const _exhaustiveCheck: never = successResult.file
                throw new Error(`Unhandled case ${JSON.stringify(successResult.file)}`)
            }
          }
        case 'RESOLVE_NOT_PRESENT':
          return normalisePathError(`Unable to find resolve path at ${importedFrom}`)
        case 'RESOLVE_SUCCESS_IGNORE_MODULE':
          return normalisePathEndsAtIgnoredDependency(importedFrom.filePath)
        default:
          const _exhaustiveCheck: never = resolutionResult
          throw new Error(`Unhandled case ${JSON.stringify(resolutionResult)}`)
      }
    }
  }
}

export function normalisePathToUnderlyingTargetForced(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  currentFilePath: string,
  elementPath: ElementPath | null,
): NormalisePathSuccess {
  return normalisePathSuccessOrThrowError(
    normalisePathToUnderlyingTarget(projectContents, nodeModules, currentFilePath, elementPath),
  )
}

export function findUnderlyingTargetComponentImplementation(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  currentFilePath: string,
  elementPath: ElementPath | null,
): UtopiaJSXComponent | null {
  const underlyingTarget = normalisePathToUnderlyingTarget(
    projectContents,
    nodeModules,
    currentFilePath,
    elementPath,
  )
  if (underlyingTarget.type === 'NORMALISE_PATH_SUCCESS') {
    const parseResult = underlyingTarget.textFile.fileContents.parsed
    if (isParseSuccess(parseResult) && underlyingTarget.normalisedPath != null) {
      const element = findJSXElementAtStaticPath(
        getUtopiaJSXComponentsFromSuccess(parseResult),
        underlyingTarget.normalisedPath,
      )
      const elementName = element?.name.baseVariable
      if (element != null && elementName != null) {
        const innerUnderlyingTarget = lookupElementImport(
          elementName,
          underlyingTarget.filePath,
          projectContents,
          nodeModules,
          element,
          underlyingTarget.normalisedPath,
          parseResult,
          { droppedPathElements: null, newPath: null },
        )
        if (
          innerUnderlyingTarget.type === 'NORMALISE_PATH_SUCCESS' &&
          isParseSuccess(innerUnderlyingTarget.textFile.fileContents.parsed)
        ) {
          return (
            innerUnderlyingTarget.textFile.fileContents.parsed.topLevelElements.find(
              (tle): tle is UtopiaJSXComponent => {
                return isUtopiaJSXComponent(tle) && tle.name === elementName
              },
            ) ?? null
          )
        }
      }
    }
  }

  return null
}
