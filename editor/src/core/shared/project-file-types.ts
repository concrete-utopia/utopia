import type { ImportType, NormalisedFrame } from 'utopia-api/core'
import type {
  ArbitraryJSBlock,
  ImportStatement,
  TopLevelElement,
  UtopiaJSXComponent,
} from './element-template'
import type { ErrorMessage } from './error-messages'
import { arrayEqualsByValue, objectEquals } from './utils'
import type {
  ImageFile,
  AssetFile,
  RevisionsStateType,
  ParsedJSONResult,
  TextFileContents,
  ParseSuccess,
  ParseFailure,
  Unparsed,
  ParsedTextFile,
  ImportAlias,
  ImportDetails,
  Imports,
  ExportFunction,
  ExportClass,
  ExportVariable,
  ExportVariables,
  ExportDestructuredAssignment,
  ExportDefaultFunctionOrClass,
  ExportDefaultIdentifier,
  ReexportWildcard,
  ReexportVariables,
  ExportDetail,
  ExportsDetail,
  ExportVariablesWithModifier,
  HighlightBounds,
  HighlightBoundsForUids,
  TextFile,
  ParsedJSONSuccess,
  ParsedJSONFailure,
  ESCodeFile,
  ESCodeFileOrigin,
  ProjectFile,
  ProjectFileType,
  NodeModules,
  ProjectContents,
  NodeModuleFile,
  PackageType,
  BaseTemplateName,
  SvgTemplateName,
  Dependencies,
  id,
  StaticElementPathPart,
  ElementPathPart,
  StaticElementPath,
  ElementPath,
  PropertyPathPart,
  PropertyPath,
  ElementPropertyPath,
} from 'utopia-shared/src/types'
import {
  imageFile,
  assetFile,
  RevisionsState,
  unparsed,
  EmptyExportsDetail,
  PinType,
} from 'utopia-shared/src/types'

export type {
  ImageFile,
  AssetFile,
  RevisionsStateType,
  ParsedJSONResult,
  TextFileContents,
  ParseSuccess,
  ParseFailure,
  Unparsed,
  ParsedTextFile,
  ImportAlias,
  ImportDetails,
  Imports,
  ExportFunction,
  ExportClass,
  ExportVariable,
  ExportVariables,
  ExportDestructuredAssignment,
  ExportDefaultFunctionOrClass,
  ExportDefaultIdentifier,
  ReexportWildcard,
  ReexportVariables,
  ExportDetail,
  ExportsDetail,
  ExportVariablesWithModifier,
  HighlightBounds,
  HighlightBoundsForUids,
  TextFile,
  ParsedJSONSuccess,
  ParsedJSONFailure,
  ESCodeFile,
  ESCodeFileOrigin,
  ProjectFile,
  ProjectFileType,
  NodeModules,
  ProjectContents,
  NodeModuleFile,
  PackageType,
  BaseTemplateName,
  SvgTemplateName,
  Dependencies,
  id,
  StaticElementPathPart,
  ElementPathPart,
  StaticElementPath,
  ElementPath,
  PropertyPathPart,
  PropertyPath,
  ElementPropertyPath,
}
export { imageFile, assetFile, RevisionsState, unparsed, EmptyExportsDetail, PinType }

export interface SceneMetadata {
  uid: string
  component: string | null
  props: { [key: string]: any }
  frame: NormalisedFrame
  label?: string
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

export function isEmptyImportDetails(details: ImportDetails): boolean {
  return (
    details.importedWithName == null &&
    details.importedFromWithin.length === 0 &&
    details.importedAs == null
  )
}

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

export function exportVariablesWithModifier(variables: Array<string>): ExportVariablesWithModifier {
  return {
    type: 'EXPORT_VARIABLES_WITH_MODIFIER',
    variables: variables,
  }
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

export function exportClass(className: string): ExportClass {
  return {
    type: 'EXPORT_CLASS',
    className: className,
  }
}

export function exportVariable(variableName: string, variableAlias: string | null): ExportVariable {
  return {
    variableName: variableName,
    variableAlias: variableAlias,
  }
}

export function exportVariables(variables: Array<ExportVariable>): ExportVariables {
  return {
    type: 'EXPORT_VARIABLES',
    variables: variables,
  }
}

export function exportDestructuredAssignment(
  variables: Array<ExportVariable>,
): ExportDestructuredAssignment {
  return {
    type: 'EXPORT_DESTRUCTURED_ASSIGNMENT',
    variables: variables,
  }
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
export interface HighlightBoundsWithFile {
  bounds: HighlightBounds
  filePath: string
}

export type HighlightBoundsWithFileForUids = { [uid: string]: HighlightBoundsWithFile }

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

export function isUnparsed(parsed: ParsedTextFile): parsed is Unparsed {
  return parsed.type === 'UNPARSED'
}

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

export function isParsedJSONSuccess(result: ParsedJSONResult): result is ParsedJSONSuccess {
  return result.type === 'SUCCESS'
}

export function isParsedJSONFailure(result: ParsedJSONResult): result is ParsedJSONFailure {
  return result.type === 'FAILURE'
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
