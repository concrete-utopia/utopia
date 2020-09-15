import { ParseSuccess, ParseResult, HighlightBoundsForUids } from '../../shared/project-file-types'
import {
  printCodeOptions,
  printCode,
  lintAndParse,
  getHighlightBoundsWithUID,
  getHighlightBoundsWithoutUID,
} from './parser-printer'
import { fastForEach } from '../../shared/utils'

interface PrintCodeMessage {
  type: 'printcode'
  parseSuccess: ParseSuccess
  stripUIDs: boolean
}

export function createPrintCodeMessage(
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
): PrintCodeMessage {
  return {
    type: 'printcode',
    parseSuccess: parseSuccess,
    stripUIDs: stripUIDs,
  }
}

interface PrintCodeResultMessage {
  type: 'printcoderesult'
  printResult: string
  highlightBounds: HighlightBoundsForUids
}

function createPrintCodeResultMessage(
  printResult: string,
  highlightBounds: HighlightBoundsForUids,
): PrintCodeResultMessage {
  return {
    type: 'printcoderesult',
    printResult: printResult,
    highlightBounds: highlightBounds,
  }
}

interface ParseFileMessage {
  type: 'parsefile'
  filename: string
  content: string
}

export function createParseFileMessage(filename: string, content: string): ParseFileMessage {
  return {
    type: 'parsefile',
    filename: filename,
    content: content,
  }
}

export interface ParseFileResultMessage {
  type: 'parsefileresult'
  parseResult: ParseResult
}

function createParseFileResultMessage(parseResult: ParseResult): ParseFileResultMessage {
  return {
    type: 'parsefileresult',
    parseResult: parseResult,
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

type ParserPrinterRequestMessage = PrintCodeMessage | ParseFileMessage
export type ParserPrinterResultMessage =
  | PrintCodeResultMessage
  | ParseFileResultMessage
  | ParsePrintFailedMessage

export function handleMessage(
  workerMessage: ParserPrinterRequestMessage,
  sendMessage: (content: ParserPrinterResultMessage) => void,
): void {
  switch (workerMessage.type) {
    case 'parsefile': {
      try {
        parseFile(workerMessage.filename, workerMessage.content, sendMessage)
      } catch (e) {
        sendMessage(createParsePrintFailedMessage())
        throw e
      }
      break
    }
    case 'printcode': {
      try {
        printCodeAsync(workerMessage.parseSuccess, workerMessage.stripUIDs, sendMessage)
      } catch (e) {
        sendMessage(createParsePrintFailedMessage())
        throw e
      }
      break
    }
  }
}

function parseFile(
  filename: string,
  content: string,
  sendMessage: (c: ParserPrinterResultMessage) => void,
) {
  const parseResult = lintAndParse(filename, content)
  sendMessage(createParseFileResultMessage(parseResult))
}

function printCodeAsync(
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  sendMessage: (content: ParserPrinterResultMessage) => void,
) {
  const withUIDs = printCode(
    printCodeOptions(false, true, true, false),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
  )
  const highlightBoundsWithUID = getHighlightBoundsWithUID('with-uids', withUIDs)

  const withoutUIDs = printCode(
    printCodeOptions(false, true, true, stripUIDs),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
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
  sendMessage(createPrintCodeResultMessage(withoutUIDs, newHighlightBounds))
}
