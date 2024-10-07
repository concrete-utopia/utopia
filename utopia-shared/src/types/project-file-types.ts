import type { ArbitraryJSBlock, TopLevelElement } from './element-template'
import type { ErrorMessage } from './error-messages'

export type id = string
enum StaticModifier {}

export type StaticElementPathPart = StaticModifier & Array<id>
export type ElementPathPart = Array<id> | StaticElementPathPart

export interface StaticElementPath {
  type: 'elementpath'
  parts: Array<StaticElementPathPart>
}

export interface ElementPath {
  type: 'elementpath'
  parts: Array<ElementPathPart>
}

export type PropertyPathPart = string | number

export type PropertyPath<T extends Array<PropertyPathPart> = Array<PropertyPathPart>> = {
  propertyElements: T
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

export type ElementPropertyPath = {
  elementPath: ElementPath
  propertyPath: PropertyPath
}

export type Dependencies = { [key: string]: ElementPropertyPath }

export enum PinType {
  Absolute = 'absolute',
  Relative = 'relative',
}

export interface ImportAlias {
  name: string
  alias: string
}

export interface ImportDetails {
  importedWithName: string | null // import name from './place'
  importedFromWithin: Array<ImportAlias> // import { name as alias } from './place'
  importedAs: string | null // import * as name from './place'
}

export type Imports = { [importSource: string]: ImportDetails }

export type ImportsMergeResolution = {
  imports: Imports
  duplicateNameMapping: Map<string, string>
}

// export let name1, name2, …, nameN; // also var, const
// export let name1 = …, name2 = …, …, nameN; // also var, const
export interface ExportVariablesWithModifier {
  type: 'EXPORT_VARIABLES_WITH_MODIFIER'
  variables: Array<string>
}

// export function functionName(){...}
export interface ExportFunction {
  type: 'EXPORT_FUNCTION'
  functionName: string
}

// export class ClassName {...}
export interface ExportClass {
  type: 'EXPORT_CLASS'
  className: string
}

// export { name1, name2, …, nameN };
// export { variable1 as name1, variable2 as name2, …, nameN };
// export { name1 as default, … };
export interface ExportVariable {
  variableName: string
  variableAlias: string | null
}

export interface ExportVariables {
  type: 'EXPORT_VARIABLES'
  variables: Array<ExportVariable>
}

// export const { name1, name2: bar } = o;
export interface ExportDestructuredAssignment {
  type: 'EXPORT_DESTRUCTURED_ASSIGNMENT'
  variables: Array<ExportVariable>
}

// export default function (…) { … } // also class, function*
// export default function name1(…) { … } // also class, function*
export interface ExportDefaultFunctionOrClass {
  type: 'EXPORT_DEFAULT_FUNCTION_OR_CLASS'
  name: string | null
}

// const App = (…) { … }
// export default App;
export interface ExportDefaultIdentifier {
  type: 'EXPORT_DEFAULT_IDENTIFIER'
  name: string
}

// export * from …; // does not set the default export
// export * as name1 from …; // Draft ECMAScript® 2O21
export interface ReexportWildcard {
  type: 'REEXPORT_WILDCARD'
  reexportedModule: string
  namespacedVariable: string | null
}

//export { name1, name2, …, nameN } from …;
//export { import1 as name1, import2 as name2, …, nameN } from …;
//export { default, … } from …;
export interface ReexportVariables {
  type: 'REEXPORT_VARIABLES'
  reexportedModule: string
  variables: Array<ExportVariable>
}

export type ExportDetail =
  | ExportVariablesWithModifier
  | ExportFunction
  | ExportClass
  | ExportVariables
  | ExportDestructuredAssignment
  | ExportDefaultFunctionOrClass
  | ExportDefaultIdentifier
  | ReexportWildcard
  | ReexportVariables

export type ExportsDetail = Array<ExportDetail>

export const EmptyExportsDetail: ExportsDetail = []

export interface HighlightBounds {
  startLine: number
  startCol: number
  endLine: number
  endCol: number
  uid: string
}

export type HighlightBoundsForUids = { [uid: string]: HighlightBounds }

export interface HighlightBoundsWithFile extends HighlightBounds {
  filePath: string
}

export type HighlightBoundsWithFileForUids = { [uid: string]: HighlightBoundsWithFile }

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ParseSuccess {
  type: 'PARSE_SUCCESS'
  imports: Imports
  topLevelElements: Array<TopLevelElement>
  highlightBounds: HighlightBoundsForUids
  fullHighlightBounds: HighlightBoundsForUids
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
  exportsDetail: ExportsDetail
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ParseFailure {
  type: 'PARSE_FAILURE'
  diagnostics: Array<ErrorMessage> | null
  parsedJSONFailure: ParsedJSONFailure | null
  errorMessage: string | null
  errorMessages: Array<ErrorMessage>
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface Unparsed {
  type: 'UNPARSED'
}

export const unparsed: Unparsed = {
  type: 'UNPARSED',
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ParsedTextFile = ParseFailure | ParseSuccess | Unparsed

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ParsedJSONSuccess {
  type: 'SUCCESS'
  value: any
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ParsedJSONFailure {
  type: 'FAILURE'
  codeSnippet: string
  reason: string
  startLine: number
  startCol: number
  endLine: number
  endCol: number
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ParsedJSONResult = ParsedJSONSuccess | ParsedJSONFailure

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface TextFileContents {
  code: string
  parsed: ParsedTextFile
  revisionsState: RevisionsStateType
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface TextFile {
  type: 'TEXT_FILE'
  fileContents: TextFileContents
  lastSavedContents: TextFileContents | null // it is null when the file is saved
  lastParseSuccess: ParseSuccess | null
  versionNumber: number
}

interface EvalResult {
  module: {
    exports: unknown
  }
}

export type ESCodeFileOrigin = 'PROJECT_CONTENTS' | 'NODE_MODULES'

export interface ESCodeFile {
  type: 'ES_CODE_FILE'
  fileContents: string
  origin: ESCodeFileOrigin
  fullPath: string
}

export interface ESRemoteDependencyPlaceholder {
  type: 'ES_REMOTE_DEPENDENCY_PLACEHOLDER'
  url: string
  downloadStarted: boolean
}

export interface Directory {
  type: 'DIRECTORY'
}

export type ProjectFile = TextFile | ImageFile | Directory | AssetFile

export type ProjectFileType = ProjectFile['type']

export type NodeModuleFile = ESCodeFile | ESRemoteDependencyPlaceholder // TODO maybe ESCodeFile is too strict, eventually we want to have ProjectFile here

export type NodeModules = {
  [filepath: string]: NodeModuleFile
}

// Key here is the full filename.
export type ProjectContents = { [filepath: string]: ProjectFile }

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ImageFile {
  type: 'IMAGE_FILE'
  imageType?: string
  base64?: string
  width?: number
  height?: number
  hash: number
  gitBlobSha?: string
}

export function imageFile(
  imageType: string | undefined,
  base64: string | undefined,
  width: number | undefined,
  height: number | undefined,
  hash: number,
  gitBlobSha: string | undefined,
): ImageFile {
  return {
    type: 'IMAGE_FILE',
    imageType: imageType,
    base64: base64,
    width: width,
    height: height,
    hash: hash,
    gitBlobSha: gitBlobSha,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface AssetFile {
  type: 'ASSET_FILE'
  base64?: string
  gitBlobSha?: string
}

export function assetFile(base64: string | undefined, gitBlobSha: string | undefined): AssetFile {
  return {
    type: 'ASSET_FILE',
    base64: base64,
    gitBlobSha: gitBlobSha,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type RevisionsStateType =
  | 'PARSED_AHEAD'
  | 'CODE_AHEAD'
  | 'BOTH_MATCH'
  | 'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT'

export const RevisionsState = {
  ParsedAhead: 'PARSED_AHEAD',
  CodeAhead: 'CODE_AHEAD',
  BothMatch: 'BOTH_MATCH',
  CodeAheadButPleaseTellVSCodeAboutIt: 'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT',
} as const
