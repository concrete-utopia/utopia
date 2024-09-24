import type { ImportType, NormalisedFrame } from 'utopia-api/core'
import type {
  ArbitraryJSBlock,
  ImportStatement,
  TopLevelElement,
  UtopiaJSXComponent,
} from './element-template'
import type { ErrorMessage } from './error-messages'
import { arrayEqualsByValue, objectEquals } from './utils'
import type { ImageFile, AssetFile, RevisionsStateType } from 'utopia-shared/src/types'
import { imageFile, assetFile, RevisionsState } from 'utopia-shared/src/types'

export type { ImageFile, AssetFile, RevisionsStateType }
export { imageFile, assetFile, RevisionsState }

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
    arrayEqualsByValue(first.importedFromWithin, second.importedFromWithin, importAliasEquals) &&
    first.importedAs === second.importedAs
  )
}

export type Imports = { [importSource: string]: ImportDetails }

export type ImportsMergeResolution = {
  imports: Imports
  duplicateNameMapping: Map<string, string>
}

export function importsResolution(
  imports: Imports,
  duplicateNameMapping: Map<string, string> = new Map<string, string>(),
): ImportsMergeResolution {
  return {
    imports: imports,
    duplicateNameMapping: duplicateNameMapping,
  }
}

export function isImportsMergeResolution(
  imports: Imports | ImportsMergeResolution,
): imports is ImportsMergeResolution {
  return imports.hasOwnProperty('imports')
}

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

export function isExportFunction(e: ExportDetail): e is ExportFunction {
  return e.type === 'EXPORT_FUNCTION'
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

export function isExportDefaultFunctionOrClass(e: ExportDetail): e is ExportDefaultFunctionOrClass {
  return e.type === 'EXPORT_DEFAULT_FUNCTION_OR_CLASS'
}

// const App = (…) { … }
// export default App;
export interface ExportDefaultIdentifier {
  type: 'EXPORT_DEFAULT_IDENTIFIER'
  name: string
}

export function exportDefaultIdentifier(name: string): ExportDefaultIdentifier {
  return {
    type: 'EXPORT_DEFAULT_IDENTIFIER',
    name: name,
  }
}

export function isExportDefaultIdentifier(e: ExportDetail): e is ExportDefaultIdentifier {
  return e.type === 'EXPORT_DEFAULT_IDENTIFIER'
}

export function isExportDefault(
  e: ExportDetail,
): e is ExportDefaultIdentifier | ExportDefaultFunctionOrClass {
  return isExportDefaultFunctionOrClass(e) || isExportDefaultIdentifier(e)
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
  | ExportDefaultIdentifier
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

export interface HighlightBoundsWithFile {
  bounds: HighlightBounds
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

export function parseSuccess(
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  bounds: HighlightBoundsForUids,
  jsxFactoryFunction: string | null,
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null,
  exportsDetail: ExportsDetail,
  fullHighlightBounds: HighlightBoundsForUids,
): ParseSuccess {
  return {
    type: 'PARSE_SUCCESS',
    imports: imports,
    topLevelElements: topLevelElements,
    highlightBounds: bounds,
    jsxFactoryFunction: jsxFactoryFunction,
    combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
    exportsDetail: exportsDetail,
    fullHighlightBounds: fullHighlightBounds,
  }
}

export function isParseSuccess(parsed: ParsedTextFile): parsed is ParseSuccess {
  return parsed.type === 'PARSE_SUCCESS'
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
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

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface Unparsed {
  type: 'UNPARSED'
}

export const unparsed: Unparsed = {
  type: 'UNPARSED',
}

export function isUnparsed(parsed: ParsedTextFile): parsed is Unparsed {
  return parsed.type === 'UNPARSED'
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
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

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ParsedJSONSuccess {
  type: 'SUCCESS'
  value: any
}

export function isParsedJSONSuccess(result: ParsedJSONResult): result is ParsedJSONSuccess {
  return result.type === 'SUCCESS'
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

export function isParsedJSONFailure(result: ParsedJSONResult): result is ParsedJSONFailure {
  return result.type === 'FAILURE'
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ParsedJSONResult = ParsedJSONSuccess | ParsedJSONFailure

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
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

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface TextFile {
  type: 'TEXT_FILE'
  fileContents: TextFileContents
  lastSavedContents: TextFileContents | null // it is null when the file is saved
  lastParseSuccess: ParseSuccess | null
  versionNumber: number
}

export function textFile(
  fileContents: TextFileContents,
  lastSavedContents: TextFileContents | null,
  lastParseSuccess: ParseSuccess | null,
  versionNumber: number,
): TextFile {
  return {
    type: 'TEXT_FILE',
    fileContents: fileContents,
    lastSavedContents: lastSavedContents,
    lastParseSuccess: lastParseSuccess,
    versionNumber: versionNumber,
  }
}

export function codeFile(
  fileContents: string,
  lastSavedContents: string | null,
  versionNumber: number = 0,
  revisionsState: RevisionsStateType = RevisionsState.CodeAhead,
): TextFile {
  return textFile(
    textFileContents(fileContents, unparsed, revisionsState),
    lastSavedContents == null
      ? null
      : textFileContents(lastSavedContents, unparsed, revisionsState),
    null,
    versionNumber,
  )
}

export function isTextFile(projectFile: ProjectFile): projectFile is TextFile {
  return projectFile.type === 'TEXT_FILE'
}

export function isParsedTextFile(projectFile: ProjectFile | null): projectFile is TextFile {
  return (
    projectFile != null && isTextFile(projectFile) && !isUnparsed(projectFile.fileContents.parsed)
  )
}

export function getParsedContentsFromTextFile(
  projectFile: ProjectFile | null,
): ParseSuccess | null {
  if (projectFile == null || !isTextFile(projectFile)) {
    return null
  }
  if (!isParseSuccess(projectFile.fileContents.parsed)) {
    return null
  }
  return projectFile.fileContents.parsed
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

export function isImageFile(projectFile: ProjectFile): projectFile is ImageFile {
  return projectFile.type === 'IMAGE_FILE'
}

export function isAssetFile(projectFile: ProjectFile | null): projectFile is AssetFile {
  return projectFile != null && projectFile.type === 'ASSET_FILE'
}

export interface Directory {
  type: 'DIRECTORY'
}

export function directory(): Directory {
  return {
    type: 'DIRECTORY',
  }
}

export function isDirectory(projectFile: ProjectFile): projectFile is Directory {
  return projectFile.type === 'DIRECTORY'
}

export type ProjectFile = TextFile | ImageFile | Directory | AssetFile

export type ProjectFileType = ProjectFile['type']

export type NodeModuleFile = ESCodeFile | ESRemoteDependencyPlaceholder // TODO maybe ESCodeFile is too strict, eventually we want to have ProjectFile here

export type NodeModules = {
  [filepath: string]: NodeModuleFile
}

// Key here is the full filename.
export type ProjectContents = { [filepath: string]: ProjectFile }
