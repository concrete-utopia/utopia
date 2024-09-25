import type { ParseSuccess } from '../../shared/project-file-types'
import type { SteganographyMode } from './parser-printer'
import { printCodeOptions, printCode, lintAndParse } from './parser-printer'
import type {
  ClearParseCacheMessage,
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
import {
  clearParseCache,
  getParseResultFromCache,
  storeParseResultInCache,
} from './parse-cache-utils.worker'
import type { ParseCacheOptions } from '../../shared/parse-cache-utils'

export async function handleMessage(
  workerMessage: ParsePrintFilesRequest | ClearParseCacheMessage,
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
                  workerMessage.filePathMappings,
                  file.content,
                  file.previousParsed,
                  file.versionNumber,
                  alreadyExistingUIDs_MUTABLE,
                  workerMessage.applySteganography,
                  workerMessage.parsingCacheOptions,
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
                  workerMessage.parsingCacheOptions,
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
    case 'clearparsecache': {
      await clearParseCache(workerMessage.parsingCacheOptions)
      break
    }
  }
}

export async function getParseFileResultWithCache(
  filename: string,
  filePathMappings: FilePathMappings,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  fileVersionNumber: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  applySteganography: SteganographyMode,
  parsingCacheOptions: ParseCacheOptions,
): Promise<ParseFileResult> {
  if (parsingCacheOptions.useParsingCache) {
    //check localforage for cache
    const cachedResult = await getParseResultFromCache(filename, content, parsingCacheOptions)
    if (cachedResult != null) {
      return cachedResult
    }
  }

  const parseResult = getParseFileResult(
    filename,
    filePathMappings,
    content,
    oldParseResultForUIDComparison,
    fileVersionNumber,
    alreadyExistingUIDs_MUTABLE,
    applySteganography,
    parsingCacheOptions,
  )

  return parseResult
}

export function getParseFileResult(
  filename: string,
  filePathMappings: FilePathMappings,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  fileVersionNumber: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  applySteganography: SteganographyMode,
  parseCacheOptions: ParseCacheOptions,
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
  const result = createParseFileResult(filename, parseResult, fileVersionNumber)
  if (result.parseResult.type === 'PARSE_SUCCESS' && parseCacheOptions.useParsingCache) {
    // non blocking cache write
    void storeParseResultInCache(filename, content, result, parseCacheOptions)
  }

  return result
}

export function getPrintAndReparseCodeResult(
  filename: string,
  filePathMappings: FilePathMappings,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  fileVersionNumber: number,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
  parseCacheOptions: ParseCacheOptions,
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
    fileVersionNumber,
    alreadyExistingUIDs,
    applySteganography,
    parseCacheOptions,
  )
  return createPrintAndReparseResult(
    filename,
    parseResult.parseResult,
    fileVersionNumber,
    printedCode,
  )
}
