import {
  ParseSuccess,
  ParsedTextFile,
  HighlightBoundsForUids,
} from '../../shared/project-file-types'
import {
  printCodeOptions,
  printCode,
  lintAndParse,
  getHighlightBoundsWithUID,
  getHighlightBoundsWithoutUID,
} from './parser-printer'
import { fastForEach } from '../../shared/utils'

interface PrintCode {
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

interface PrintCodeResult {
  type: 'printcoderesult'
  filename: string
  printResult: string
  highlightBounds: HighlightBoundsForUids
  lastRevisedTime: number
}

function createPrintCodeResult(
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

interface ParseFile {
  type: 'parsefile'
  filename: string
  content: string
  lastRevisedTime: number
}

export function createParseFile(
  filename: string,
  content: string,
  lastRevisedTime: number,
): ParseFile {
  return {
    type: 'parsefile',
    filename: filename,
    content: content,
    lastRevisedTime: lastRevisedTime,
  }
}

export interface ParseFileResult {
  type: 'parsefileresult'
  filename: string
  parseResult: ParsedTextFile
  lastRevisedTime: number
}

function createParseFileResult(
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

export interface ParsePrintFailedMessage {
  type: 'parseprintfailed'
}

function createParsePrintFailedMessage(): ParsePrintFailedMessage {
  return {
    type: 'parseprintfailed',
  }
}

export type ParseOrPrint = PrintCode | ParseFile

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

export type ParsePrintResultMessage = ParsePrintFilesResult | ParsePrintFailedMessage

export function handleMessage(
  workerMessage: ParsePrintFilesRequest,
  sendMessage: (content: ParsePrintResultMessage) => void,
): void {
  switch (workerMessage.type) {
    case 'parseprintfiles': {
      try {
        const results = workerMessage.files.map((file) => {
          switch (file.type) {
            case 'parsefile':
              return getParseFileResult(file.filename, file.content, file.lastRevisedTime)
            case 'printcode':
              return getPrintCodeResult(
                file.filename,
                file.parseSuccess,
                file.stripUIDs,
                file.lastRevisedTime,
              )
            default:
              const _exhaustiveCheck: never = file
              throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
          }
        })
        sendMessage(createParsePrintFilesResult(results))
      } catch (e) {
        sendMessage(createParsePrintFailedMessage())
        throw e
      }
      break
    }
  }
}

function getParseFileResult(
  filename: string,
  content: string,
  lastRevisedTime: number,
): ParseFileResult {
  const parseResult = lintAndParse(filename, content)
  return createParseFileResult(filename, parseResult, lastRevisedTime)
}

function getPrintCodeResult(
  filename: string,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  lastRevisedTime: number,
): PrintCodeResult {
  const withUIDs = printCode(
    printCodeOptions(false, true, true, false),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )
  const highlightBoundsWithUID = getHighlightBoundsWithUID('with-uids', withUIDs)

  const withoutUIDs = printCode(
    printCodeOptions(false, true, true, stripUIDs),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )
  const highlightBoundsWithoutUID = getHighlightBoundsWithoutUID('without-uids', withoutUIDs)

  let newHighlightBounds: HighlightBoundsForUids = {}
  if (highlightBoundsWithUID.length === highlightBoundsWithoutUID.length) {
    fastForEach(highlightBoundsWithUID, (withUID, index) => {
      const withoutUID = highlightBoundsWithoutUID[index]
      newHighlightBounds[withUID.uid] = {
        ...withoutUID,
        uid: withUID.uid,
      }
    })
  }
  return createPrintCodeResult(filename, withoutUIDs, newHighlightBounds, lastRevisedTime)
}
