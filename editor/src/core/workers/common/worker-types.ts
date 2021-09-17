import { ProjectContentTreeRoot } from '../../../components/assets'
import { ErrorMessage } from '../../shared/error-messages'
import { TypeDefinitions } from '../../shared/npm-dependency-types'
import {
  TextFile,
  ParseSuccess,
  HighlightBoundsForUids,
  ParsedTextFile,
  ProjectFile,
} from '../../shared/project-file-types'
import { RawSourceMap } from '../ts/ts-typings/RawSourceMap'

export type FileContent = string | TextFile

export interface PrintCode {
  type: 'printcode'
  filename: string
  parseSuccess: ParseSuccess
  stripUIDs: boolean
  lastRevisedTime: number
}

export function createPrintCode(
  filename: string,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  lastRevisedTime: number,
): PrintCode {
  return {
    type: 'printcode',
    filename: filename,
    parseSuccess: parseSuccess,
    stripUIDs: stripUIDs,
    lastRevisedTime: lastRevisedTime,
  }
}

export interface ParseFile {
  type: 'parsefile'
  filename: string
  content: string
  previousParsed: ParseSuccess | null
  lastRevisedTime: number
}

export function createParseFile(
  filename: string,
  content: string,
  previousParsed: ParseSuccess | null,
  lastRevisedTime: number,
): ParseFile {
  return {
    type: 'parsefile',
    filename: filename,
    content: content,
    previousParsed: previousParsed,
    lastRevisedTime: lastRevisedTime,
  }
}

export type ParseOrPrint = PrintCode | ParseFile

export interface PrintCodeResult {
  type: 'printcoderesult'
  filename: string
  printResult: string
  highlightBounds: HighlightBoundsForUids
  lastRevisedTime: number
}

export function createPrintCodeResult(
  filename: string,
  printResult: string,
  highlightBounds: HighlightBoundsForUids,
  lastRevisedTime: number,
): PrintCodeResult {
  return {
    type: 'printcoderesult',
    filename: filename,
    printResult: printResult,
    highlightBounds: highlightBounds,
    lastRevisedTime: lastRevisedTime,
  }
}

export interface ParseFileResult {
  type: 'parsefileresult'
  filename: string
  parseResult: ParsedTextFile
  lastRevisedTime: number
}

export function createParseFileResult(
  filename: string,
  parseResult: ParsedTextFile,
  lastRevisedTime: number,
): ParseFileResult {
  return {
    type: 'parsefileresult',
    filename: filename,
    parseResult: parseResult,
    lastRevisedTime: lastRevisedTime,
  }
}
export type ParseOrPrintResult = PrintCodeResult | ParseFileResult

export interface ParsePrintFilesResult {
  type: 'parseprintfilesresult'
  files: Array<ParseOrPrintResult>
}

export function createParsePrintFilesResult(
  files: Array<ParseOrPrintResult>,
): ParsePrintFilesResult {
  return {
    type: 'parseprintfilesresult',
    files: files,
  }
}

export interface ParsePrintFailedMessage {
  type: 'parseprintfailed'
}

export function createParsePrintFailedMessage(): ParsePrintFailedMessage {
  return {
    type: 'parseprintfailed',
  }
}

export type ParsePrintResultMessage = ParsePrintFilesResult | ParsePrintFailedMessage

export interface ParsePrintFilesRequest {
  type: 'parseprintfiles'
  files: Array<ParseOrPrint>
}

export function createParsePrintFilesRequest(files: Array<ParseOrPrint>): ParsePrintFilesRequest {
  return {
    type: 'parseprintfiles',
    files: files,
  }
}

export function getParseResult(
  workers: UtopiaTsWorkers,
  files: Array<ParseOrPrint>,
): Promise<Array<ParseOrPrintResult>> {
  return new Promise((resolve, reject) => {
    const handleMessage = (e: MessageEvent) => {
      const data = e.data as ParsePrintResultMessage
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

    workers.addParserPrinterEventListener(handleMessage)
    workers.sendParsePrintMessage(files)
  })
}

export interface SingleFileBuildResult {
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
  errors: Array<ErrorMessage>
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

export interface DetailedTypeInfo {
  name: string
  memberInfo: { type: string; members: { [member: string]: string } }
}

export type ExportType = {
  type: string
  functionInfo: Array<DetailedTypeInfo> | null
  reactClassInfo: DetailedTypeInfo | null
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
  sendParsePrintMessage: (files: Array<ParseOrPrint>) => void
  sendLinterRequestMessage: (filename: string, content: string) => void
  addParserPrinterEventListener: (handler: (e: MessageEvent) => void) => void
  removeParserPrinterEventListener: (handler: (e: MessageEvent) => void) => void
  addLinterResultEventListener: (handler: (e: MessageEvent) => void) => void
  removeLinterResultEventListener: (handler: (e: MessageEvent) => void) => void
  initWatchdogWorker(projectID: string): void
  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void
  sendHeartbeatResponseMessage: (id: NodeJS.Timer, projectId: string, safeMode: boolean) => void
}
