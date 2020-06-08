import * as TS from 'typescript'
import { FlexParentProps, LayoutSystem, NormalisedFrame } from 'utopia-api'
import { Either, Left, Right, isRight, isLeft } from './either'
import { TopLevelElement, UtopiaJSXComponent } from './element-template'
import { ErrorMessage } from './error-messages'

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

export interface ImportDetails {
  importedWithName: string | null
  importedFromWithin: Array<ImportAlias>
  importedAs: string | null
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

export type Imports = { [importSource: string]: ImportDetails }

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
  dependencyOrdering: Array<string>
  jsxFactoryFunction: string | null
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

// Key here is the full filename.
export type ProjectContents = { [key: string]: ProjectFile }

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
