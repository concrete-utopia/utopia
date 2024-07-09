import type { ParseSuccess } from '../../shared/project-file-types'
import type { SteganographyMode } from './parser-printer'
import { printCodeOptions, printCode, lintAndParse } from './parser-printer'
import type {
  ParseFileResult,
  ParsePrintFilesRequest,
  ParsePrintResultMessage,
  PrintAndReparseResult,
} from '../common/worker-types'
import {
  createParseFileResult,
  createParsePrintFailedMessage,
  createParsePrintFilesResult,
  createPrintAndReparseResult,
} from '../common/worker-types'
import type { FilePathMappings } from '../common/project-file-utils'

export function handleMessage(
  workerMessage: ParsePrintFilesRequest,
  sendMessage: (content: ParsePrintResultMessage) => void,
): void {
  switch (workerMessage.type) {
    case 'parseprintfiles': {
      try {
        const alreadyExistingUIDs_MUTABLE: Set<string> = new Set(workerMessage.alreadyExistingUIDs)
        const results = workerMessage.files.map((file) => {
          switch (file.type) {
            case 'parsefile':
              return getParseFileResult(
                file.filename,
                workerMessage.filePathMappings,
                file.content,
                file.previousParsed,
                file.versionNumber,
                alreadyExistingUIDs_MUTABLE,
                workerMessage.applySteganography,
              )
            case 'printandreparsefile':
              return getPrintAndReparseCodeResult(
                file.filename,
                workerMessage.filePathMappings,
                file.parseSuccess,
                file.stripUIDs,
                file.versionNumber,
                alreadyExistingUIDs_MUTABLE,
                workerMessage.applySteganography,
              )
            default:
              const _exhaustiveCheck: never = file
              throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
          }
        })
        sendMessage(createParsePrintFilesResult(results, workerMessage.messageID))
      } catch (e) {
        sendMessage(createParsePrintFailedMessage(workerMessage.messageID))
        throw e
      }
      break
    }
  }
}

export function getParseFileResult(
  filename: string,
  filePathMappings: FilePathMappings,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  versionNumber: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  applySteganography: SteganographyMode,
): ParseFileResult {
  const parseResult = lintAndParse(
    filename,
    filePathMappings,
    content,
    oldParseResultForUIDComparison,
    alreadyExistingUIDs_MUTABLE,
    'trim-bounds',
    applySteganography,
  )
  return createParseFileResult(filename, parseResult, versionNumber)
}

export function getPrintAndReparseCodeResult(
  filename: string,
  filePathMappings: FilePathMappings,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  versionNumber: number,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): PrintAndReparseResult {
  const printedCode = printCode(
    filename,
    printCodeOptions(false, true, true, stripUIDs),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )

  const parseResult = getParseFileResult(
    filename,
    filePathMappings,
    printedCode,
    parseSuccess,
    versionNumber,
    alreadyExistingUIDs,
    applySteganography,
  )
  return createPrintAndReparseResult(filename, parseResult.parseResult, versionNumber, printedCode)
}
