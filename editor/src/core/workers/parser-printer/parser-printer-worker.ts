import type { ParseSuccess } from '../../shared/project-file-types'
import type { SteganographyMode, StripUIDsBehavior } from './parser-printer'
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
                file.content,
                file.previousParsed,
                file.versionNumber,
                alreadyExistingUIDs_MUTABLE,
                workerMessage.applySteganography,
              )
            case 'printandreparsefile':
              return getPrintAndReparseCodeResult(
                file.filename,
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
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  versionNumber: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  applySteganography: SteganographyMode,
): ParseFileResult {
  const parseResult = lintAndParse(
    filename,
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
  parseSuccess: ParseSuccess,
  stripUIDs: StripUIDsBehavior,
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
    printedCode,
    parseSuccess,
    versionNumber,
    alreadyExistingUIDs,
    applySteganography,
  )
  return createPrintAndReparseResult(filename, parseResult.parseResult, versionNumber, printedCode)
}
