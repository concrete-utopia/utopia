import { ParseSuccess } from '../../shared/project-file-types'
import { printCodeOptions, printCode, lintAndParse } from './parser-printer'
import {
  ParseFileResult,
  createParseFileResult,
  createParsePrintFailedMessage,
  createParsePrintFilesResult,
  ParsePrintFilesRequest,
  ParsePrintResultMessage,
  PrintAndReparseResult,
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
                file.lastRevisedTime,
                alreadyExistingUIDs_MUTABLE,
              )
            case 'printandreparsefile':
              return getPrintAndReparseCodeResult(
                file.filename,
                file.parseSuccess,
                file.stripUIDs,
                file.lastRevisedTime,
                alreadyExistingUIDs_MUTABLE,
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

function getParseFileResult(
  filename: string,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  lastRevisedTime: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
): ParseFileResult {
  const parseResult = lintAndParse(
    filename,
    content,
    oldParseResultForUIDComparison,
    alreadyExistingUIDs_MUTABLE,
  )
  return createParseFileResult(filename, parseResult, lastRevisedTime)
}

function getPrintAndReparseCodeResult(
  filename: string,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  lastRevisedTime: number,
  alreadyExistingUIDs: Set<string>,
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
    lastRevisedTime,
    alreadyExistingUIDs,
  )
  return createPrintAndReparseResult(
    filename,
    parseResult.parseResult,
    lastRevisedTime,
    {},
    printedCode,
  )
}
