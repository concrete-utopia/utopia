import { ImportType, NormalisedFrame } from 'utopia-api/core'
import {
  ArbitraryJSBlock,
  ImportStatement,
  JSXAttribute,
  JSXAttributeOtherJavaScript,
  TopLevelElement,
} from './element-template'
import { ErrorMessage } from './error-messages'
import { arrayEquals, objectEquals } from './utils'

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

export type ElementPropertyPath = {
  elementPath: ElementPath
  propertyPath: PropertyPath
}

export type Dependencies = { [key: string]: ElementPropertyPath }

export enum PinType {
  Absolute = 'absolute',
  Relative = 'relative',
}

export interface SceneMetadata {
  uid: string
  component: string | null
  props: { [key: string]: any }
  frame: NormalisedFrame
  label?: string
}

export interface ImportAlias {
  name: string
  alias: string
}

export function importAlias(name: string, alias?: string): ImportAlias {
  return {
    name: name,
    alias: alias == null ? name : alias,
  }
}

export function importAliasEquals(first: ImportAlias, second: ImportAlias): boolean {
  return first.name === second.name && first.alias === second.alias
}

export interface ImportDetails {
  importedWithName: string | null // import name from './place'
  importedFromWithin: Array<ImportAlias> // import { name as alias } from './place'
  importedAs: string | null // import * as name from './place'
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

export function isImportSideEffects(target: ImportDetails): boolean {
  // import './place'
  return (
    target.importedWithName == null &&
    target.importedFromWithin.length === 0 &&
    target.importedAs == null
  )
}

export function importStatementFromImportDetails(
  moduleName: string,
  details: ImportDetails,
): ImportStatement {
  let importParts: Array<string> = []
  if (details.importedWithName != null) {
    importParts.push(details.importedWithName)
  }
  if (details.importedAs != null) {
    importParts.push(`* as ${details.importedAs}`)
  }
  if (details.importedFromWithin.length > 0) {
    let importedFromWithinParts: Array<string> = []
    for (const fromWithin of details.importedFromWithin) {
      if (fromWithin.name === fromWithin.alias) {
        importedFromWithinParts.push(fromWithin.name)
      } else {
        importedFromWithinParts.push(`${fromWithin.name} as ${fromWithin.alias}`)
      }
      importParts.push(`{ ${importedFromWithinParts.join(', ')} }`)
    }
  }
  const rawCode = `import ${importParts.join(', ')} from '${moduleName}'`
  return {
    type: 'IMPORT_STATEMENT',
    rawCode: rawCode,
    importStarAs: details.importedAs != null,
    importWithName: details.importedWithName != null,
    imports: details.importedFromWithin.map((i) => i.name),
    module: moduleName,
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

export function importDetailsFromImportOption(importOption: ImportType): ImportDetails {
  const importedWithName = importOption.type === 'default' ? importOption.name : null
  const importedAs = importOption.type === 'star' ? importOption.name : null
  const importedFromWithin =
    importOption.type == null && importOption.name != null
      ? [{ name: importOption.name, alias: importOption.name }]
      : []
  return importDetails(importedWithName, importedFromWithin, importedAs)
}

// export let name1, name2, …, nameN; // also var, const
// export let name1 = …, name2 = …, …, nameN; // also var, const
export interface ExportVariablesWithModifier {
  type: 'EXPORT_VARIABLES_WITH_MODIFIER'
  variables: Array<string>
}

export function exportVariablesWithModifier(variables: Array<string>): ExportVariablesWithModifier {
  return {
    type: 'EXPORT_VARIABLES_WITH_MODIFIER',
    variables: variables,
  }
}

// export function functionName(){...}
export interface ExportFunction {
  type: 'EXPORT_FUNCTION'
  functionName: string
}

export function exportFunction(functionName: string): ExportFunction {
  return {
    type: 'EXPORT_FUNCTION',
    functionName: functionName,
  }
}

// export class ClassName {...}
export interface ExportClass {
  type: 'EXPORT_CLASS'
  className: string
}

export function exportClass(className: string): ExportClass {
  return {
    type: 'EXPORT_CLASS',
    className: className,
  }
}

// export { name1, name2, …, nameN };
// export { variable1 as name1, variable2 as name2, …, nameN };
// export { name1 as default, … };
export interface ExportVariable {
  variableName: string
  variableAlias: string | null
}

export function exportVariable(variableName: string, variableAlias: string | null): ExportVariable {
  return {
    variableName: variableName,
    variableAlias: variableAlias,
  }
}

export interface ExportVariables {
  type: 'EXPORT_VARIABLES'
  variables: Array<ExportVariable>
}

export function exportVariables(variables: Array<ExportVariable>): ExportVariables {
  return {
    type: 'EXPORT_VARIABLES',
    variables: variables,
  }
}

// export const { name1, name2: bar } = o;
export interface ExportDestructuredAssignment {
  type: 'EXPORT_DESTRUCTURED_ASSIGNMENT'
  variables: Array<ExportVariable>
}

export function exportDestructuredAssignment(
  variables: Array<ExportVariable>,
): ExportDestructuredAssignment {
  return {
    type: 'EXPORT_DESTRUCTURED_ASSIGNMENT',
    variables: variables,
  }
}

// export default function (…) { … } // also class, function*
// export default function name1(…) { … } // also class, function*
export interface ExportDefaultFunctionOrClass {
  type: 'EXPORT_DEFAULT_FUNCTION_OR_CLASS'
  name: string | null
}

export function exportDefaultFunctionOrClass(name: string | null): ExportDefaultFunctionOrClass {
  return {
    type: 'EXPORT_DEFAULT_FUNCTION_OR_CLASS',
    name: name,
  }
}

// const App = (…) { … }
// export default App;
export interface ExportIdentifier {
  type: 'EXPORT_IDENTIFIER'
  name: string
}

export function exportIdentifier(name: string): ExportIdentifier {
  return {
    type: 'EXPORT_IDENTIFIER',
    name: name,
  }
}

// export * from …; // does not set the default export
// export * as name1 from …; // Draft ECMAScript® 2O21
export interface ReexportWildcard {
  type: 'REEXPORT_WILDCARD'
  reexportedModule: string
  namespacedVariable: string | null
}

export function reexportWildcard(
  reexportedModule: string,
  namespacedVariable: string | null,
): ReexportWildcard {
  return {
    type: 'REEXPORT_WILDCARD',
    reexportedModule: reexportedModule,
    namespacedVariable: namespacedVariable,
  }
}

//export { name1, name2, …, nameN } from …;
//export { import1 as name1, import2 as name2, …, nameN } from …;
//export { default, … } from …;
export interface ReexportVariables {
  type: 'REEXPORT_VARIABLES'
  reexportedModule: string
  variables: Array<ExportVariable>
}

export function reexportVariables(
  reexportedModule: string,
  variables: Array<ExportVariable>,
): ReexportVariables {
  return {
    type: 'REEXPORT_VARIABLES',
    reexportedModule: reexportedModule,
    variables: variables,
  }
}

export type ExportDetail =
  | ExportVariablesWithModifier
  | ExportFunction
  | ExportClass
  | ExportVariables
  | ExportDestructuredAssignment
  | ExportDefaultFunctionOrClass
  | ExportIdentifier
  | ReexportWildcard
  | ReexportVariables

export type ExportsDetail = Array<ExportDetail>

export const EmptyExportsDetail: ExportsDetail = []

export function mergeExportsDetail(first: ExportsDetail, second: ExportsDetail): ExportsDetail {
  return [...first, ...second]
}

export function isExportDestructuredAssignment(
  exportDetail: ExportDetail,
): exportDetail is ExportDestructuredAssignment {
  return exportDetail.type === 'EXPORT_DESTRUCTURED_ASSIGNMENT'
}

export function isReexportExportDetail(
  exportDetail: ExportDetail,
): exportDetail is ReexportWildcard | ReexportVariables {
  return exportDetail.type === 'REEXPORT_WILDCARD' || exportDetail.type === 'REEXPORT_VARIABLES'
}

export interface HighlightBounds {
  startLine: number
  startCol: number
  endLine: number
  endCol: number
  uid: string
}

export function highlightBounds(
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
  uid: string,
): HighlightBounds {
  return {
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
    uid: uid,
  }
}

export type HighlightBoundsForUids = { [uid: string]: HighlightBounds }

export interface HighlightBoundsWithFile extends HighlightBounds {
  filePath: string
}

export type HighlightBoundsWithFileForUids = { [uid: string]: HighlightBoundsWithFile }

export interface ParseSuccess {
  type: 'PARSE_SUCCESS'
  imports: Imports
  topLevelElements: Array<TopLevelElement>
  highlightBounds: HighlightBoundsForUids
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
  exportsDetail: ExportsDetail
}

export function parseSuccess(
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  bounds: HighlightBoundsForUids,
  jsxFactoryFunction: string | null,
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null,
  exportsDetail: ExportsDetail,
): ParseSuccess {
  return {
    type: 'PARSE_SUCCESS',
    imports: imports,
    topLevelElements: topLevelElements,
    highlightBounds: bounds,
    jsxFactoryFunction: jsxFactoryFunction,
    combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
    exportsDetail: exportsDetail,
  }
}

export function isParseSuccess(parsed: ParsedTextFile): parsed is ParseSuccess {
  return parsed.type === 'PARSE_SUCCESS'
}

export interface ParseFailure {
  type: 'PARSE_FAILURE'
  diagnostics: Array<ErrorMessage> | null
  parsedJSONFailure: ParsedJSONFailure | null
  errorMessage: string | null
  errorMessages: Array<ErrorMessage>
}

export function parseFailure(
  diagnostics: Array<ErrorMessage> | null,
  parsedJSON: ParsedJSONFailure | null,
  errorMessage: string | null,
  errorMessages: Array<ErrorMessage>,
): ParseFailure {
  return {
    type: 'PARSE_FAILURE',
    diagnostics: diagnostics,
    parsedJSONFailure: parsedJSON,
    errorMessage: errorMessage,
    errorMessages: errorMessages,
  }
}

export function isParseFailure(parsed: ParsedTextFile): parsed is ParseFailure {
  return parsed.type === 'PARSE_FAILURE'
}

export interface Unparsed {
  type: 'UNPARSED'
}

export const unparsed: Unparsed = {
  type: 'UNPARSED',
}

export function isUnparsed(parsed: ParsedTextFile): parsed is Unparsed {
  return parsed.type === 'UNPARSED'
}

export type ParsedTextFile = ParseFailure | ParseSuccess | Unparsed

export function foldParsedTextFile<X>(
  foldFailure: (failure: ParseFailure) => X,
  foldSuccess: (success: ParseSuccess) => X,
  foldUnparsed: (unparsed: Unparsed) => X,
  file: ParsedTextFile,
): X {
  switch (file.type) {
    case 'PARSE_FAILURE':
      return foldFailure(file)
    case 'PARSE_SUCCESS':
      return foldSuccess(file)
    case 'UNPARSED':
      return foldUnparsed(file)
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled type ${JSON.stringify(file)}`)
  }
}

export function mapParsedTextFile(
  transform: (success: ParseSuccess) => ParseSuccess,
  file: ParsedTextFile,
): ParsedTextFile {
  if (file.type === 'PARSE_SUCCESS') {
    return transform(file)
  } else {
    return file
  }
}

export function forEachParseSuccess(
  action: (success: ParseSuccess) => void,
  file: ParsedTextFile,
): void {
  if (file.type === 'PARSE_SUCCESS') {
    return action(file)
  }
}

export interface ParsedJSONSuccess {
  type: 'SUCCESS'
  value: any
}

export function isParsedJSONSuccess(result: ParsedJSONResult): result is ParsedJSONSuccess {
  return result.type === 'SUCCESS'
}

export interface ParsedJSONFailure {
  type: 'FAILURE'
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

export type RevisionsStateType = 'PARSED_AHEAD' | 'CODE_AHEAD' | 'BOTH_MATCH'

export const RevisionsState = {
  ParsedAhead: 'PARSED_AHEAD',
  CodeAhead: 'CODE_AHEAD',
  BothMatch: 'BOTH_MATCH',
} as const

// Ensure this is kept up to date with server/src/Utopia/Web/ClientModel.hs.
export interface TextFileContents {
  code: string
  parsed: ParsedTextFile
  revisionsState: RevisionsStateType
}

export function textFileContents(
  code: string,
  parsed: ParsedTextFile,
  revisionsState: RevisionsStateType,
): TextFileContents {
  return {
    code: code,
    parsed: parsed,
    revisionsState: revisionsState,
  }
}

// Ensure this is kept up to date with server/src/Utopia/Web/ClientModel.hs.
export interface TextFile {
  type: 'TEXT_FILE'
  fileContents: TextFileContents
  lastSavedContents: TextFileContents | null // it is null when the file is saved
  lastParseSuccess: ParseSuccess | null
  lastRevisedTime: number
}

export function textFile(
  fileContents: TextFileContents,
  lastSavedContents: TextFileContents | null,
  lastParseSuccess: ParseSuccess | null,
  lastRevisedTime: number,
): TextFile {
  return {
    type: 'TEXT_FILE',
    fileContents: fileContents,
    lastSavedContents: lastSavedContents,
    lastParseSuccess: lastParseSuccess,
    lastRevisedTime: lastRevisedTime,
  }
}

export function codeFile(fileContents: string, lastSavedContents: string | null): TextFile {
  return textFile(
    textFileContents(fileContents, unparsed, RevisionsState.CodeAhead),
    lastSavedContents == null
      ? null
      : textFileContents(lastSavedContents, unparsed, RevisionsState.CodeAhead),
    null,
    0,
  )
}

export function isTextFile(projectFile: ProjectFile | null): projectFile is TextFile {
  return projectFile != null && projectFile.type === 'TEXT_FILE'
}

export function isParsedTextFile(projectFile: ProjectFile | null): projectFile is TextFile {
  return isTextFile(projectFile) && !isUnparsed(projectFile.fileContents.parsed)
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

export function esCodeFile(
  fileContents: string,
  origin: ESCodeFileOrigin,
  fullPath: string,
): ESCodeFile {
  return {
    type: 'ES_CODE_FILE',
    fileContents: fileContents,
    origin: origin,
    fullPath: fullPath,
  }
}

export function isEsCodeFile(projectFile: any): projectFile is ESCodeFile {
  return projectFile != null && projectFile.type === 'ES_CODE_FILE'
}

export interface ESRemoteDependencyPlaceholder {
  type: 'ES_REMOTE_DEPENDENCY_PLACEHOLDER'
  url: string
  downloadStarted: boolean
}

export function esRemoteDependencyPlaceholder(
  url: string,
  downloadStarted: boolean,
): ESRemoteDependencyPlaceholder {
  return {
    type: 'ES_REMOTE_DEPENDENCY_PLACEHOLDER',
    url: url,
    downloadStarted: downloadStarted,
  }
}

export function isEsRemoteDependencyPlaceholder(
  projectFile: any,
): projectFile is ESRemoteDependencyPlaceholder {
  return projectFile != null && projectFile.type === 'ES_REMOTE_DEPENDENCY_PLACEHOLDER'
}

// Ensure this is kept up to date with server/src/Utopia/Web/ClientModel.hs.
export interface ImageFile {
  type: 'IMAGE_FILE'
  imageType?: string
  base64?: string
  width?: number
  height?: number
  hash: number
}

export function imageFile(
  imageType: string | undefined,
  base64: string | undefined,
  width: number | undefined,
  height: number | undefined,
  hash: number,
): ImageFile {
  return {
    type: 'IMAGE_FILE',
    imageType: imageType,
    base64: base64,
    width: width,
    height: height,
    hash: hash,
  }
}

// Ensure this is kept up to date with server/src/Utopia/Web/ClientModel.hs.
export interface AssetFile {
  type: 'ASSET_FILE'
  base64?: string
}

export function assetFile(base64: string | undefined): AssetFile {
  return {
    type: 'ASSET_FILE',
    base64: base64,
  }
}

export function isAssetFile(projectFile: ProjectFile | null): projectFile is AssetFile {
  return projectFile != null && projectFile.type === 'ASSET_FILE'
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

export type ElementOriginType =
  // Completely statically defined element with a known single place in the hierarchy.
  | 'statically-defined'
  // An element generated from within some arbitrary code, but for which we have access to the definition.
  | 'generated-static-definition-present'
  // Something from somewhere, for which we probably have access to the bounds.
  | 'unknown-element'

export function isUnknownOrGeneratedElement(elementOriginType: ElementOriginType): boolean {
  return (
    elementOriginType === 'unknown-element' ||
    elementOriginType === 'generated-static-definition-present'
  )
}
