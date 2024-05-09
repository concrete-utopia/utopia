import type { ParseSuccess, ParsedTextFile } from '../../shared/project-file-types'
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
import localforage from 'localforage'

export async function handleMessage(
  workerMessage: ParsePrintFilesRequest,
  sendMessage: (content: ParsePrintResultMessage) => void,
): Promise<void> {
  switch (workerMessage.type) {
    case 'parseprintfiles': {
      try {
        const alreadyExistingUIDs_MUTABLE: Set<string> = new Set(workerMessage.alreadyExistingUIDs)
        const results = await Promise.all(
          workerMessage.files.map(async (file) => {
            switch (file.type) {
              case 'parsefile':
                return getParseFileResultWithCache(
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
          }),
        )
        sendMessage(createParsePrintFilesResult(results, workerMessage.messageID))
      } catch (e) {
        sendMessage(createParsePrintFailedMessage(workerMessage.messageID))
        throw e
      }
      break
    }
  }
}

function getCacheKey(filename: string, content: string, versionNumber: number): string {
  return `${filename}::${content}::${versionNumber}`
}

async function getParseFileResultWithCache(
  filename: string,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  versionNumber: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  applySteganography: SteganographyMode,
  checkCacheFirst: boolean = true,
): Promise<ParseFileResult> {
  const cacheKey = getCacheKey(filename, content, versionNumber)
  if (checkCacheFirst) {
    //check localforage for cache
    const cachedResult = await localforage.getItem<ParseFileResult>(cacheKey)
    if (cachedResult?.parseResult?.type === 'PARSE_SUCCESS') {
      return cachedResult
    }
  }

  const parseResult = getParseFileResult(
    filename,
    content,
    oldParseResultForUIDComparison,
    versionNumber,
    alreadyExistingUIDs_MUTABLE,
    applySteganography,
  )

  return parseResult
}

function getParseFileResult(
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
  const result = createParseFileResult(filename, parseResult, versionNumber)
  if (result.parseResult.type === 'PARSE_SUCCESS') {
    // non blocking cache write
    const cacheKey = getCacheKey(filename, content, versionNumber)
    void localforage.setItem(cacheKey, result)
  }

  return result
}

export function getPrintAndReparseCodeResult(
  filename: string,
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
    printedCode,
    parseSuccess,
    versionNumber,
    alreadyExistingUIDs,
    applySteganography,
  )
  return createPrintAndReparseResult(filename, parseResult.parseResult, versionNumber, printedCode)
}
