import type {
  ElementPath,
  Imports,
  NodeModules,
  StaticElementPath,
  TextFile,
} from '../../core/shared/project-file-types'
import {
  esCodeFile,
  getParsedContentsFromTextFile,
  isEsCodeFile,
  isParseSuccess,
  isTextFile,
} from '../../core/shared/project-file-types'
import type { RawSourceMap } from '../../core/workers/ts/ts-typings/RawSourceMap'
import type { EmitFileResult } from '../../core/workers/ts/ts-worker'
import Utils from '../../utils/utils'
import type {
  PreferredChildComponentDescriptor,
  PropertyControls,
} from './internal-property-controls'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { resolveModulePath } from '../../core/es-modules/package-manager/module-resolution'
import type { EvaluationCache } from '../../core/es-modules/package-manager/package-manager'
import { getCurriedEditorRequireFn } from '../../core/es-modules/package-manager/package-manager'
import { getAllUniqueUids } from '../../core/model/get-unique-ids'
import type { Either } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import type {
  ImportInfo,
  JSXConditionalExpressionWithoutUID,
  JSXElementWithoutUID,
  JSXFragmentWithoutUID,
  JSXMapExpressionWithoutUID,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import {
  clearJSXConditionalExpressionWithoutUIDUniqueIDs,
  clearJSXElementWithoutUIDUniqueIDs,
  clearJSXFragmentWithoutUIDUniqueIDs,
  clearJSXMapExpressionWithoutUIDUniqueIDs,
} from '../../core/shared/element-template'
import { objectMap } from '../../core/shared/object-utils'
import { getTransitiveReverseDependencies } from '../../core/shared/project-contents-dependencies'
import { assertNever, fastForEach } from '../../core/shared/utils'
import type {
  ExportType,
  ExportsInfo,
  MultiFileBuildResult,
} from '../../core/workers/common/worker-types'
import type { ProjectContentTreeRoot } from '../assets'
import { getProjectFileByFilePath } from '../assets'
import type { EditorDispatch } from '../editor/action-types'
import type { Emphasis, Focus, Icon, InspectorSpec, StyleSectionState } from 'utopia-api'

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

export type ComponentElementToInsert =
  | JSXElementWithoutUID
  | JSXConditionalExpressionWithoutUID
  | JSXFragmentWithoutUID
  | JSXMapExpressionWithoutUID

export function clearComponentElementToInsertUniqueIDs(
  toInsert: ComponentElementToInsert,
): ComponentElementToInsert {
  switch (toInsert.type) {
    case 'JSX_ELEMENT':
      return clearJSXElementWithoutUIDUniqueIDs(toInsert)
    case 'JSX_CONDITIONAL_EXPRESSION':
      return clearJSXConditionalExpressionWithoutUIDUniqueIDs(toInsert)
    case 'JSX_FRAGMENT':
      return clearJSXFragmentWithoutUIDUniqueIDs(toInsert)
    case 'JSX_MAP_EXPRESSION':
      return clearJSXMapExpressionWithoutUIDUniqueIDs(toInsert)
    default:
      assertNever(toInsert)
  }
}

export function componentElementToInsertHasChildren(toInsert: ComponentElementToInsert): boolean {
  switch (toInsert.type) {
    case 'JSX_ELEMENT':
    case 'JSX_FRAGMENT':
      return toInsert.children.length > 0
    case 'JSX_MAP_EXPRESSION':
    case 'JSX_CONDITIONAL_EXPRESSION':
      // More in the conceptual sense than actual sense of having a field containing children.
      return true
    default:
      assertNever(toInsert)
  }
}

export interface ComponentInfo {
  insertMenuLabel: string
  elementToInsert: () => ComponentElementToInsert
  importsToAdd: Imports
}

export function componentInfo(
  insertMenuLabel: string,
  elementToInsert: () => ComponentElementToInsert,
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
  supportsChildren: boolean
  preferredChildComponents: Array<PreferredChildComponentDescriptor>
  variants: ComponentInfo[]
  source: ComponentDescriptorSource
  focus: Focus
  inspector: InspectorSpec
  emphasis: Emphasis
  icon: Icon
  label: string | null
  future_styleSection: StyleSectionState | null
}

export const ComponentDescriptorDefaults: Pick<
  ComponentDescriptor,
  'focus' | 'inspector' | 'emphasis' | 'icon' | 'label' | 'future_styleSection'
> = {
  focus: 'default',
  inspector: [],
  emphasis: 'regular',
  icon: 'component',
  label: null,
  future_styleSection: null,
}

export function componentDescriptor(
  properties: PropertyControls,
  supportsChildren: boolean,
  variants: Array<ComponentInfo>,
  preferredChildComponents: Array<PreferredChildComponentDescriptor>,
  source: ComponentDescriptorSource,
  focus: Focus,
  inspector: InspectorSpec,
  emphasis: Emphasis,
  icon: Icon,
  label: string | null,
  styleSectioState: StyleSectionState | null,
): ComponentDescriptor {
  return {
    properties: properties,
    supportsChildren: supportsChildren,
    variants: variants,
    preferredChildComponents: preferredChildComponents,
    source: source,
    focus: focus,
    inspector: inspector,
    emphasis: emphasis,
    icon: icon,
    label: label,
    future_styleSection: styleSectioState,
  }
}

export type ComponentDescriptorSource =
  | DefaultComponentDescriptor
  | ComponentDescriptorFromDescriptorFile

interface DefaultComponentDescriptor {
  type: 'DEFAULT'
}

export function defaultComponentDescriptor(): DefaultComponentDescriptor {
  return {
    type: 'DEFAULT',
  }
}

export function isDefaultComponentDescriptor(
  desc: ComponentDescriptorSource,
): desc is DefaultComponentDescriptor {
  return desc.type === 'DEFAULT'
}

export interface ComponentDescriptorFromDescriptorFile {
  type: 'DESCRIPTOR_FILE'
  sourceDescriptorFile: string
}

export function componentDescriptorFromDescriptorFile(
  sourceDescriptorFile: string,
): ComponentDescriptorFromDescriptorFile {
  return {
    type: 'DESCRIPTOR_FILE',
    sourceDescriptorFile: sourceDescriptorFile,
  }
}

export function isComponentDescriptorFromDescriptorFile(
  desc: ComponentDescriptorSource,
): desc is ComponentDescriptorFromDescriptorFile {
  return desc.type === 'DESCRIPTOR_FILE'
}

export interface ComponentDescriptorWithName extends ComponentDescriptor {
  componentName: string
  moduleName: string // if local module, then it's always absolute path without extension
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
      const projectContentsFile = getProjectFileByFilePath(projectContents, moduleKey)
      const origin = projectContentsFile == null ? 'NODE_MODULES' : 'PROJECT_CONTENTS'
      nodeModules[moduleKey] = esCodeFile(modulesFile.transpiledCode, origin, moduleKey)
    }
  })
  // Cleanup non-existant project files.
  fastForEach(Object.keys(nodeModules), (moduleKey) => {
    const modulesFile = nodeModules[moduleKey]
    if (isEsCodeFile(modulesFile)) {
      if (modulesFile.origin === 'PROJECT_CONTENTS') {
        const projectContentsFile = getProjectFileByFilePath(projectContents, moduleKey)
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

export interface NormalisePathElementNotFound {
  type: 'NORMALISE_PATH_ELEMENT_NOT_FOUND'
  elementPathString: string
}

export function normalisePathElementNotFound(
  elementPathString: string,
): NormalisePathElementNotFound {
  return {
    type: 'NORMALISE_PATH_ELEMENT_NOT_FOUND',
    elementPathString: elementPathString,
  }
}

export type NormalisePathResult =
  | NormalisePathElementNotFound
  | NormalisePathError
  | NormalisePathSuccess

export function normalisePathSuccessOrThrowError(
  normalisePathResult: NormalisePathResult,
): NormalisePathSuccess {
  switch (normalisePathResult.type) {
    case 'NORMALISE_PATH_SUCCESS':
      return normalisePathResult
    case 'NORMALISE_PATH_ELEMENT_NOT_FOUND':
      throw new Error(`Could not find element with path ${normalisePathResult.elementPathString}`)
    case 'NORMALISE_PATH_ERROR':
      throw new Error(`Could not proceed: ${normalisePathResult.errorMessage}.`)
    default:
      const _exhaustiveCheck: never = normalisePathResult
      throw new Error(`Unhandled case ${JSON.stringify(normalisePathResult)}`)
  }
}

export function normalisePathToUnderlyingTarget(
  projectContents: ProjectContentTreeRoot,
  elementPath: ElementPath | null,
): NormalisePathResult {
  if (elementPath == null || EP.isEmptyPath(elementPath)) {
    return normalisePathError('Empty element path')
  }

  const staticPath = EP.dynamicPathToStaticPath(elementPath)
  const lastPartOfPath = EP.takeLastPartOfPath(elementPath)

  const allUidsWithFiles = getAllUniqueUids(projectContents)
  const filePathFromUID = allUidsWithFiles.uidsToFilePaths[EP.toUid(staticPath)]
  const fileFromUID =
    filePathFromUID == null ? null : getProjectFileByFilePath(projectContents, filePathFromUID)

  if (filePathFromUID == null || fileFromUID == null) {
    return normalisePathElementNotFound(EP.toString(elementPath))
  }

  if (!isTextFile(fileFromUID) || !isParseSuccess(fileFromUID.fileContents.parsed)) {
    // This shouldn't happen, since getAllUniqueUids only reads from parsed files, but we'll
    // keep this here in case that changes
    return normalisePathError(
      `Error retrieving ${EP.toString(elementPath)} from file ${filePathFromUID}`,
    )
  }

  return normalisePathSuccess(
    EP.dynamicPathToStaticPath(lastPartOfPath),
    filePathFromUID,
    fileFromUID,
    lastPartOfPath,
  )
}

export function findUnderlyingTargetComponentImplementationFromImportInfo(
  projectContents: ProjectContentTreeRoot,
  importInfo: ImportInfo | null,
): UtopiaJSXComponent | null {
  if (importInfo == null) {
    return null
  }

  const variableName =
    importInfo.type === 'SAME_FILE_ORIGIN' ? importInfo.variableName : importInfo.exportedName

  // we have to find the element based on the top level name
  const file = getProjectFileByFilePath(projectContents, importInfo.filePath)
  const parsedContents = getParsedContentsFromTextFile(file)
  if (parsedContents == null) {
    return null
  }

  const foundTopLevelElement = parsedContents.topLevelElements.find(
    (tle): tle is UtopiaJSXComponent =>
      tle.type === 'UTOPIA_JSX_COMPONENT' && tle.name === variableName,
  )

  if (foundTopLevelElement == null) {
    return null
  }

  return foundTopLevelElement
}
