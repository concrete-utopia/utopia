import * as TS from 'typescript'
import { FlexParentProps, LayoutSystem, NormalisedFrame } from 'utopia-api'
import { Either, Left, Right, isRight, isLeft } from './either'
import { ArbitraryJSBlock, TopLevelElement, UtopiaJSXComponent } from './element-template'
import { ErrorMessage } from './error-messages'
import { arrayEquals, objectEquals } from './utils'

export type id = string

export type ScenePath = {
  type: 'scenepath'
  sceneElementPath: StaticElementPath
}
export type StaticElementPath = StaticModifier & Array<id>
export type ElementPath = Array<id> | StaticElementPath
export type InstancePath = {
  scene: ScenePath
  element: ElementPath
}
enum StaticModifier {}
export type StaticInstancePath = {
  scene: ScenePath
  element: StaticElementPath
}

export type StaticTemplatePath = ScenePath | StaticInstancePath
export type TemplatePath = StaticTemplatePath | InstancePath

export type PropertyPathPart = string | number

export type PropertyPath = {
  propertyElements: Array<PropertyPathPart>
}

export type PackageType = 'base' | 'svg' | 'app'
export type BaseTemplateName =
  | 'output'
  | 'view'
  | 'multi-generator'
  | 'placeholder'
  | 'image'
  | 'text'
  | 'rectangle'
  | 'ellipse'
  | 'custom-code'
  | 'code-component'
export type SvgTemplateName = 'arc' | 'circle' | 'ellipse' | 'path' | 'polygon' | 'rect'

export type TemplatePropertyPath = {
  templatePath: TemplatePath
  propertyPath: PropertyPath
}

export type Dependencies = { [key: string]: TemplatePropertyPath }

export const enum PinType {
  Absolute = 'absolute',
  Relative = 'relative',
}

export interface ElementCanvasMetadata {}

export type CanvasElementMetadataMap = { [utopiaID: string]: ElementCanvasMetadata }

// KILLME CanvasMetadata is dead
export interface ScenePinnedContainer {
  layoutSystem: LayoutSystem.PinSystem
}

export type SceneContainer = ScenePinnedContainer

export interface SceneMetadata {
  uid: string
  component: string | null
  props: { [key: string]: any }
  frame: NormalisedFrame
  container: SceneContainer
  label?: string
}

export interface CanvasMetadata {}

export type CanvasMetadataRightBeforePrinting = {
  scenes: Array<Omit<SceneMetadata, 'uid'>> | null
  elementMetadata: CanvasElementMetadataMap
}

export type PrintedCanvasMetadata = {
  scenes: Array<SceneMetadata> | null
  elementMetadata: CanvasElementMetadataMap
}

export interface ImportAlias {
  name: string
  alias: string
}

export function importAlias(name: string, alias?: string) {
  return {
    name: name,
    alias: alias == null ? name : alias,
  }
}

export function importAliasEquals(first: ImportAlias, second: ImportAlias): boolean {
  return first.name === second.name && first.alias === second.alias
}

export interface ImportDetails {
  importedWithName: string | null
  importedFromWithin: Array<ImportAlias>
  importedAs: string | null
}

export function importDetails(
  importedWithName: string | null,
  importedFromWithin: Array<ImportAlias>,
  importedAs: string | null,
): ImportDetails {
  return {
    importedWithName: importedWithName,
    importedFromWithin: importedFromWithin,
    importedAs: importedAs,
  }
}

export function importDetailsEquals(first: ImportDetails, second: ImportDetails): boolean {
  return (
    first.importedWithName === second.importedWithName &&
    arrayEquals(first.importedFromWithin, second.importedFromWithin, importAliasEquals) &&
    first.importedAs === second.importedAs
  )
}

export type Imports = { [importSource: string]: ImportDetails }

export function importsEquals(first: Imports, second: Imports): boolean {
  return objectEquals(first, second, importDetailsEquals)
}

export interface HighlightBounds {
  startLine: number
  startCol: number
  endLine: number
  endCol: number
  uid: string
}

export type HighlightBoundsForUids = { [uid: string]: HighlightBounds }

export type CanvasMetadataParseResult = Either<unknown, CanvasMetadata>

export interface ParseSuccess {
  imports: Imports
  topLevelElements: Array<TopLevelElement>
  canvasMetadata: CanvasMetadataParseResult
  projectContainedOldSceneMetadata: boolean
  code: string
  highlightBounds: HighlightBoundsForUids
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
}

export function isParseSuccess(result: ParseResult): result is Right<ParseSuccess> {
  return isRight(result)
}

export interface ParseFailure {
  diagnostics: Array<TS.Diagnostic> | null
  parsedJSONFailure: ParsedJSONFailure | null
  errorMessage: string | null
  errorMessages: Array<ErrorMessage>
  code: string
}

export function isParseFailure(result: ParseResult): result is Left<ParseFailure> {
  return isLeft(result)
}

export type ParseResult = Either<ParseFailure, ParseSuccess>

export interface ParsedJSONSuccess {
  type: 'SUCCESS'
  value: any
}

export function isParsedJSONSuccess(result: ParsedJSONResult): result is ParsedJSONSuccess {
  return result.type === 'SUCCESS'
}

export interface ParsedJSONFailure {
  type: 'FAILURE'
  errorNode: TS.Node
  codeSnippet: string
  reason: string
  startLine: number
  startCol: number
  endLine: number
  endCol: number
}

export function isParsedJSONFailure(result: ParsedJSONResult): result is ParsedJSONFailure {
  return result.type === 'FAILURE'
}

export type ParsedJSONResult = ParsedJSONSuccess | ParsedJSONFailure

export const enum RevisionsState {
  ParsedAhead = 'PARSED_AHEAD',
  CodeAhead = 'CODE_AHEAD',
  BothMatch = 'BOTH_MATCH',
}

export interface UIJSFile {
  type: 'UI_JS_FILE'
  fileContents: ParseResult
  lastSavedContents: ParseResult | null // it is null when the file is saved
  revisionsState: RevisionsState
  lastRevisedTime: number
}

export function isUIJSFile(projectFile: ProjectFile | null): projectFile is UIJSFile {
  return projectFile != null && projectFile.type === 'UI_JS_FILE'
}

export interface CodeFile {
  type: 'CODE_FILE'
  fileContents: string // Maybe should be more like an asset?
  lastSavedContents: string | null // it is null when the file is saved
}

export function isCodeFile(projectFile: ProjectFile | null): projectFile is CodeFile {
  return projectFile != null && projectFile.type === 'CODE_FILE'
}

export function isCodeOrUiJsFile(file: ProjectFile): file is CodeFile | UIJSFile {
  return file.type === 'CODE_FILE' || file.type === 'UI_JS_FILE'
}

interface EvalResult {
  module: {
    exports: unknown
  }
}

export interface ESCodeFile {
  type: 'ES_CODE_FILE'
  fileContents: string
  evalResultCache: EvalResult | null
}

export function esCodeFile(fileContents: string, evalResultCache: EvalResult | null): ESCodeFile {
  return {
    type: 'ES_CODE_FILE',
    fileContents: fileContents,
    evalResultCache: evalResultCache,
  }
}

export function isEsCodeFile(projectFile: any): projectFile is ESCodeFile {
  return projectFile != null && projectFile.type === 'ES_CODE_FILE'
}

export interface ESRemoteDependencyPlaceholder {
  type: 'ES_REMOTE_DEPENDENCY_PLACEHOLDER'
  packagename: string
  version: string
  downloadStarted: boolean
}

export function esRemoteDependencyPlaceholder(
  packageName: string,
  version: string,
  downloadStarted: boolean,
): ESRemoteDependencyPlaceholder {
  return {
    type: 'ES_REMOTE_DEPENDENCY_PLACEHOLDER',
    packagename: packageName,
    version: version,
    downloadStarted: downloadStarted,
  }
}

export function isEsRemoteDependencyPlaceholder(
  projectFile: any,
): projectFile is ESRemoteDependencyPlaceholder {
  return projectFile != null && projectFile.type === 'ES_REMOTE_DEPENDENCY_PLACEHOLDER'
}

export interface ImageFile {
  type: 'IMAGE_FILE'
  imageType?: string
  base64?: string
  width?: number
  height?: number
  hash: string
}

export interface AssetFile {
  type: 'ASSET_FILE'
}

export function isAssetFile(projectFile: ProjectFile | null): projectFile is AssetFile {
  return projectFile != null && projectFile.type === 'ASSET_FILE'
}

export interface Directory {
  type: 'DIRECTORY'
}

export type ProjectFile = UIJSFile | CodeFile | ImageFile | Directory | AssetFile

export type ProjectFileType = ProjectFile['type']

export type NodeModuleFile = ESCodeFile | ESRemoteDependencyPlaceholder // TODO maybe ESCodeFile is too strict, eventually we want to have ProjectFile here

export type NodeModules = {
  [filepath: string]: NodeModuleFile
}

// Key here is the full filename.
export type ProjectContents = { [filepath: string]: ProjectFile }

export type ElementOriginType =
  // Mostly self explanatory, this is a scene.
  | 'scene'
  // Completely statically defined element with a known single place in the hierarchy.
  | 'statically-defined'
  // An element generated from within some arbitrary code, but for which we have access to the definition.
  | 'generated-static-definition-present'
  // Something from somewhere, for which we probably have access to the bounds.
  | 'unknown-element'

export type LayoutWrapper = 'Layoutable' | 'Positionable' | 'Resizeable'
