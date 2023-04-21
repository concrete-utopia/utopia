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
  boundsAreValid,
} from './parser-printer'
import { fastForEach } from '../../shared/utils'
import { emptySet } from '../../shared/set-utils'
import {
  PrintCode,
  PrintCodeResult,
  ParseFile,
  ParseFileResult,
  ParseOrPrint,
  ParseOrPrintResult,
  ParsePrintFailedMessage,
  ParsePrintFilesResult,
  createParseFileResult,
  createParsePrintFailedMessage,
  createParsePrintFilesResult,
  createPrintCodeResult,
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
            case 'printcode':
              return getPrintCodeResult(
                file.filename,
                file.parseSuccess,
                file.stripUIDs,
                file.lastRevisedTime,
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
    'trim-bounds',
  )
  return createParseFileResult(filename, parseResult, lastRevisedTime)
}

function getPrintCodeResult(
  filename: string,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  lastRevisedTime: number,
): PrintCodeResult {
  const withUIDs = printCode(
    filename,
    printCodeOptions(false, true, true, false),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )
  const highlightBoundsWithUID = getHighlightBoundsWithUID('with-uids', withUIDs)

  const withoutUIDs = printCode(
    filename,
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
      if (boundsAreValid(withUID.uid)) {
        const withoutUID = highlightBoundsWithoutUID[index]
        newHighlightBounds[withUID.uid] = {
          ...withoutUID,
          uid: withUID.uid,
        }
      }
    })
  }
  return createPrintCodeResult(filename, withoutUIDs, newHighlightBounds, lastRevisedTime)
}

function getPrintAndReparseCodeResult(
  filename: string,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  lastRevisedTime: number,
  alreadyExistingUIDs: Set<string>,
): PrintAndReparseResult {
  const printResult = getPrintCodeResult(filename, parseSuccess, stripUIDs, lastRevisedTime)
  const parseResult = getParseFileResult(
    filename,
    printResult.printResult,
    parseSuccess,
    lastRevisedTime,
    alreadyExistingUIDs,
  )
  return createPrintAndReparseResult(
    filename,
    parseResult.parseResult,
    lastRevisedTime,
    printResult.highlightBounds,
    printResult.printResult,
  )
}
