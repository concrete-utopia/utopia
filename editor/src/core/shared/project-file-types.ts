import * as TS from 'typescript'
import { NormalisedFrame } from 'utopia-api'
import { ArbitraryJSBlock, ImportStatement, TopLevelElement } from './element-template'
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

export const enum PinType {
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

export interface ExportDetailNamed {
  type: 'EXPORT_DETAIL_NAMED'
  name: string
  moduleName: string | undefined
}

export function exportDetailNamed(name: string, moduleName: string | undefined): ExportDetailNamed {
  return {
    type: 'EXPORT_DETAIL_NAMED',
    name: name,
    moduleName: moduleName,
  }
}

export interface ExportDetailModifier {
  type: 'EXPORT_DETAIL_MODIFIER'
}

export function exportDetailModifier(): ExportDetailModifier {
  return {
    type: 'EXPORT_DETAIL_MODIFIER',
  }
}

export interface ExportDefaultNamed {
  type: 'EXPORT_DEFAULT_NAMED'
  name: string
}

export function exportDefaultNamed(name: string): ExportDefaultNamed {
  return {
    type: 'EXPORT_DEFAULT_NAMED',
    name: name,
  }
}

export interface ExportDefaultModifier {
  type: 'EXPORT_DEFAULT_MODIFIER'
  name: string
}

export function exportDefaultModifier(name: string): ExportDefaultModifier {
  return {
    type: 'EXPORT_DEFAULT_MODIFIER',
    name: name,
  }
}

export type ExportDetail = ExportDetailNamed | ExportDetailModifier
export type ExportDefault = ExportDefaultNamed | ExportDefaultModifier

export function isExportDetailNamed(detail: ExportDetail): detail is ExportDetailNamed {
  return detail.type === 'EXPORT_DETAIL_NAMED'
}

export function isExportDetailModifier(detail: ExportDetail): detail is ExportDetailModifier {
  return detail.type === 'EXPORT_DETAIL_MODIFIER'
}

export function isExportDefaultNamed(detail: ExportDefault): detail is ExportDefaultNamed {
  return detail.type === 'EXPORT_DEFAULT_NAMED'
}

export function isExportDefaultModifier(detail: ExportDefault): detail is ExportDefaultModifier {
  return detail.type === 'EXPORT_DEFAULT_MODIFIER'
}

export interface ExportsDetail {
  defaultExport: ExportDefault | null
  namedExports: Record<string, ExportDetail>
}

export function exportsDetail(
  defaultExport: ExportDefault | null,
  namedExports: Record<string, ExportDetail>,
): ExportsDetail {
  return {
    defaultExport: defaultExport,
    namedExports: namedExports,
  }
}

export const EmptyExportsDetail: ExportsDetail = exportsDetail(null, {})

export function mergeExportsDetail(first: ExportsDetail, second: ExportsDetail): ExportsDetail {
  return {
    defaultExport: second.defaultExport ?? first.defaultExport,
    namedExports: {
      ...first.namedExports,
      ...second.namedExports,
    },
  }
}

export function addNamedExportToDetail(
  detail: ExportsDetail,
  name: string,
  alias: string,
  moduleName: string | undefined,
): ExportsDetail {
  return {
    defaultExport: detail.defaultExport,
    namedExports: {
      ...detail.namedExports,
      [name]: exportDetailNamed(alias, moduleName),
    },
  }
}

export function addModifierExportToDetail(detail: ExportsDetail, name: string): ExportsDetail {
  return {
    defaultExport: detail.defaultExport,
    namedExports: {
      ...detail.namedExports,
      [name]: exportDetailModifier(),
    },
  }
}

export function setNamedDefaultExportInDetail(detail: ExportsDetail, name: string): ExportsDetail {
  return {
    defaultExport: exportDefaultNamed(name),
    namedExports: detail.namedExports,
  }
}

export function setModifierDefaultExportInDetail(
  detail: ExportsDetail,
  name: string,
): ExportsDetail {
  return {
    defaultExport: exportDefaultModifier(name),
    namedExports: detail.namedExports,
  }
}

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

export interface ParseSuccess {
  type: 'PARSE_SUCCESS'
  imports: Imports
  topLevelElements: Array<TopLevelElement>
  highlightBounds: HighlightBoundsForUids
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
  exportsDetail: ExportsDetail
}

export function isParseSuccess(parsed: ParsedTextFile): parsed is ParseSuccess {
  return parsed.type === 'PARSE_SUCCESS'
}

export interface ParseFailure {
  type: 'PARSE_FAILURE'
  diagnostics: Array<TS.Diagnostic> | null
  parsedJSONFailure: ParsedJSONFailure | null
  errorMessage: string | null
  errorMessages: Array<ErrorMessage>
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

export interface TextFileContents {
  code: string
  parsed: ParsedTextFile
  revisionsState: RevisionsState
}

export function textFileContents(
  code: string,
  parsed: ParsedTextFile,
  revisionsState: RevisionsState,
): TextFileContents {
  return {
    code: code,
    parsed: parsed,
    revisionsState: revisionsState,
  }
}

export interface TextFile {
  type: 'TEXT_FILE'
  fileContents: TextFileContents
  lastSavedContents: TextFileContents | null // it is null when the file is saved
  lastRevisedTime: number
}

export function textFile(
  fileContents: TextFileContents,
  lastSavedContents: TextFileContents | null,
  lastRevisedTime: number,
): TextFile {
  return {
    type: 'TEXT_FILE',
    fileContents: fileContents,
    lastSavedContents: lastSavedContents,
    lastRevisedTime: lastRevisedTime,
  }
}

export function codeFile(fileContents: string, lastSavedContents: string | null): TextFile {
  return textFile(
    textFileContents(fileContents, unparsed, RevisionsState.CodeAhead),
    lastSavedContents == null
      ? null
      : textFileContents(lastSavedContents, unparsed, RevisionsState.CodeAhead),
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

export interface ImageFile {
  type: 'IMAGE_FILE'
  imageType?: string
  base64?: string
  width?: number
  height?: number
  hash: number
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
