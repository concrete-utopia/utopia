import type { ProjectContentTreeRoot } from '../../../components/assets'
import type { ErrorMessage } from '../../shared/error-messages'
import type { TypeDefinitions } from '../../shared/npm-dependency-types'
import type {
  TextFile,
  ParseSuccess,
  ParsedTextFile,
  ProjectFile,
} from '../../shared/project-file-types'
import type { SteganographyMode, StripUIDsBehavior } from '../parser-printer/parser-printer'
import type { RawSourceMap } from '../ts/ts-typings/RawSourceMap'

export type FileContent = string | TextFile

export interface ParseFile {
  type: 'parsefile'
  filename: string
  content: string
  previousParsed: ParseSuccess | null
  versionNumber: number
}

export function createParseFile(
  filename: string,
  content: string,
  previousParsed: ParseSuccess | null,
  versionNumber: number,
): ParseFile {
  return {
    type: 'parsefile',
    filename: filename,
    content: content,
    previousParsed: previousParsed,
    versionNumber: versionNumber,
  }
}

export interface PrintAndReparseFile {
  type: 'printandreparsefile'
  filename: string
  parseSuccess: ParseSuccess
  stripUIDs: StripUIDsBehavior
  versionNumber: number
}

export function createPrintAndReparseFile(
  filename: string,
  parseSuccess: ParseSuccess,
  stripUIDs: StripUIDsBehavior,
  versionNumber: number,
): PrintAndReparseFile {
  return {
    type: 'printandreparsefile',
    filename: filename,
    parseSuccess: parseSuccess,
    stripUIDs: stripUIDs,
    versionNumber: versionNumber,
  }
}

export interface ParsePrintBase {
  messageID: number
}

export type ParseOrPrint = ParseFile | PrintAndReparseFile

export interface ParseFileResult {
  type: 'parsefileresult'
  filename: string
  parseResult: ParsedTextFile
  versionNumber: number
}

export function createParseFileResult(
  filename: string,
  parseResult: ParsedTextFile,
  versionNumber: number,
): ParseFileResult {
  return {
    type: 'parsefileresult',
    filename: filename,
    parseResult: parseResult,
    versionNumber: versionNumber,
  }
}

export interface PrintAndReparseResult {
  type: 'printandreparseresult'
  filename: string
  parseResult: ParsedTextFile
  versionNumber: number
  printResult: string
}

export function createPrintAndReparseResult(
  filename: string,
  parseResult: ParsedTextFile,
  versionNumber: number,
  printResult: string,
): PrintAndReparseResult {
  return {
    type: 'printandreparseresult',
    filename: filename,
    parseResult: parseResult,
    versionNumber: versionNumber,
    printResult: printResult,
  }
}

export type ParseOrPrintResult = ParseFileResult | PrintAndReparseResult

export interface ParsePrintFilesResult extends ParsePrintBase {
  type: 'parseprintfilesresult'
  files: Array<ParseOrPrintResult>
}

export function createParsePrintFilesResult(
  files: Array<ParseOrPrintResult>,
  messageID: number,
): ParsePrintFilesResult {
  return {
    type: 'parseprintfilesresult',
    files: files,
    messageID: messageID,
  }
}

export interface ParsePrintFailedMessage extends ParsePrintBase {
  type: 'parseprintfailed'
}

export function createParsePrintFailedMessage(messageID: number): ParsePrintFailedMessage {
  return {
    type: 'parseprintfailed',
    messageID: messageID,
  }
}

export type ParsePrintResultMessage = ParsePrintFilesResult | ParsePrintFailedMessage

export interface ParsePrintFilesRequest extends ParsePrintBase {
  type: 'parseprintfiles'
  files: Array<ParseOrPrint>
  alreadyExistingUIDs: Set<string>
  applySteganography: SteganographyMode
}

export function createParsePrintFilesRequest(
  files: Array<ParseOrPrint>,
  alreadyExistingUIDs: Set<string>,
  messageID: number,
  applySteganography: SteganographyMode,
): ParsePrintFilesRequest {
  return {
    type: 'parseprintfiles',
    files: files,
    alreadyExistingUIDs: alreadyExistingUIDs,
    messageID: messageID,
    applySteganography: applySteganography,
  }
}

let PARSE_PRINT_MESSAGE_COUNTER: number = 0

export function getParseResult(
  workers: UtopiaTsWorkers,
  files: Array<ParseOrPrint>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Promise<Array<ParseOrPrintResult>> {
  const messageIDForThisRequest = PARSE_PRINT_MESSAGE_COUNTER++
  return new Promise((resolve, reject) => {
    const handleMessage = (e: MessageEvent) => {
      const data = e.data as ParsePrintResultMessage
      // Ensure that rapidly fired requests are distinguished between the handlers.
      if (data.messageID === messageIDForThisRequest) {
        switch (data.type) {
          case 'parseprintfilesresult': {
            resolve(data.files)
            workers.removeParserPrinterEventListener(handleMessage)
            break
          }
          case 'parseprintfailed': {
            reject()
            workers.removeParserPrinterEventListener(handleMessage)
            break
          }
        }
      }
    }

    workers.addParserPrinterEventListener(handleMessage)
    workers.sendParsePrintMessage(
      createParsePrintFilesRequest(
        files,
        alreadyExistingUIDs,
        messageIDForThisRequest,
        applySteganography,
      ),
    )
  })
}

export interface SingleFileBuildResult {
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
  errors: Array<ErrorMessage>
}

export function singleFileBuildResult(
  transpiledCode: string | null,
  sourceMap: RawSourceMap | null,
  errors: Array<ErrorMessage>,
): SingleFileBuildResult {
  return {
    transpiledCode: transpiledCode,
    sourceMap: sourceMap,
    errors: errors,
  }
}

export interface MultiFileBuildResult {
  [filename: string]: SingleFileBuildResult
}

export type BuildType = 'full-build' | 'incremental'

export interface ExportsInfo {
  filename: string
  code: string
  exportTypes: { [name: string]: ExportType }
}

export function exportsInfo(
  filename: string,
  code: string,
  exportTypes: { [name: string]: ExportType },
): ExportsInfo {
  return {
    filename: filename,
    code: code,
    exportTypes: exportTypes,
  }
}

export interface DetailedTypeInfoMemberInfo {
  type: string
  members: { [member: string]: string }
}

export function detailedTypeInfoMemberInfo(
  type: string,
  members: { [member: string]: string },
): DetailedTypeInfoMemberInfo {
  return {
    type: type,
    members: members,
  }
}

export interface DetailedTypeInfo {
  name: string
  memberInfo: DetailedTypeInfoMemberInfo
}

export function detailedTypeInfo(
  name: string,
  memberInfo: DetailedTypeInfoMemberInfo,
): DetailedTypeInfo {
  return {
    name: name,
    memberInfo: memberInfo,
  }
}

export interface ExportType {
  type: string
  functionInfo: Array<DetailedTypeInfo> | null
  reactClassInfo: DetailedTypeInfo | null
}

export function exportType(
  type: string,
  functionInfo: Array<DetailedTypeInfo> | null,
  reactClassInfo: DetailedTypeInfo | null,
): ExportType {
  return {
    type: type,
    functionInfo: functionInfo,
    reactClassInfo: reactClassInfo,
  }
}

export interface BuildResultMessage {
  type: 'build'
  exportsInfo: ReadonlyArray<ExportsInfo>
  buildResult: MultiFileBuildResult
  jobID: string
  buildType: BuildType
}

export interface UpdateProcessedMessage {
  type: 'updateprocessed'
  jobID: string
}

export interface InitCompleteMessage {
  type: 'initcomplete'
  jobID: string
}

export type OutgoingWorkerMessage =
  | BuildResultMessage
  | UpdateProcessedMessage
  | InitCompleteMessage

export interface UpdateFileMessage {
  type: 'updatefile'
  filename: string
  content: string | ProjectFile
  jobID: string
}

export function createUpdateFileMessage(
  filename: string,
  content: string | TextFile,
  jobID: string,
): UpdateFileMessage {
  return {
    type: 'updatefile',
    filename: filename,
    content: content,
    jobID: jobID,
  }
}

export interface InitTSWorkerMessage {
  type: 'inittsworker'
  typeDefinitions: TypeDefinitions
  projectContents: ProjectContentTreeRoot
  buildOrParsePrint: 'build' | 'parse-print'
  jobID: string
}

export function createInitTSWorkerMessage(
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContentTreeRoot,
  buildOrParsePrint: 'build' | 'parse-print',
  jobID: string,
): InitTSWorkerMessage {
  return {
    type: 'inittsworker',
    typeDefinitions: typeDefinitions,
    projectContents: projectContents,
    buildOrParsePrint: buildOrParsePrint,
    jobID: jobID,
  }
}

export interface UtopiaTsWorkers {
  sendParsePrintMessage: (request: ParsePrintFilesRequest) => void
  sendLinterRequestMessage: (filename: string, content: string) => void
  addParserPrinterEventListener: (handler: (e: MessageEvent) => void) => void
  removeParserPrinterEventListener: (handler: (e: MessageEvent) => void) => void
  addLinterResultEventListener: (handler: (e: MessageEvent) => void) => void
  removeLinterResultEventListener: (handler: (e: MessageEvent) => void) => void
  initWatchdogWorker(projectID: string): void
  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void
  sendHeartbeatResponseMessage: (id: NodeJS.Timer, projectId: string, safeMode: boolean) => void
}
