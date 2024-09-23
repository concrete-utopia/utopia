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
import localforage from 'localforage'
import type { FilePathMappings } from '../common/project-file-utils'
import type { ParseCacheOptions } from '../../../core/shared/parse-cache-utils'

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
  }
}

function getCacheKey(filename: string, versionNumber: number): string {
  const devVer = 1 // TEMP - use it for hard cache invalidation if needed now as we're developing this feature
  return `${filename}::${versionNumber}::${devVer}`
}

function logCacheMessage(message: string, parsingCacheOptions: ParseCacheOptions) {
  if (parsingCacheOptions.verboseLogCache) {
    console.info(`[PARSING CACHE] ${message}`)
  }
}

export async function getParseFileResultWithCache(
  filename: string,
  filePathMappings: FilePathMappings,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  versionNumber: number,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  applySteganography: SteganographyMode,
  parsingCacheOptions: ParseCacheOptions,
): Promise<ParseFileResult> {
  if (parsingCacheOptions.useParsingCache) {
    //check localforage for cache
    const cachedResult = await getParseResultFromCache(filename, content, versionNumber)
    if (cachedResult != null) {
      logCacheMessage(`Cache hit for ${filename}`, parsingCacheOptions)
      return cachedResult
    }
    logCacheMessage(`Cache miss for ${filename}`, parsingCacheOptions)
  }

  const parseResult = getParseFileResult(
    filename,
    filePathMappings,
    content,
    oldParseResultForUIDComparison,
    versionNumber,
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
  versionNumber: number,
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
  const result = createParseFileResult(filename, parseResult, versionNumber)
  if (result.parseResult.type === 'PARSE_SUCCESS' && parseCacheOptions.useParsingCache) {
    // non blocking cache write
    storeParseResultInCache(filename, content, versionNumber, result, parseCacheOptions)
  }

  return result
}

type CachedParseResult = { [fileContent: string]: ParseFileResult }

async function getParseResultFromCache(
  filename: string,
  content: string,
  versionNumber: number,
): Promise<ParseFileResult | null> {
  const cacheKey = getCacheKey(filename, versionNumber)
  //check localforage for cache
  const cachedResult = await localforage.getItem<CachedParseResult>(cacheKey)
  const cachedResultForContent = cachedResult?.[content]
  if (cachedResultForContent?.parseResult?.type === 'PARSE_SUCCESS') {
    return cachedResultForContent
  }
  return null
}

function storeParseResultInCache(
  filename: string,
  content: string,
  versionNumber: number,
  result: ParseFileResult,
  parsingCacheOptions: ParseCacheOptions,
) {
  const cacheKey = getCacheKey(filename, versionNumber)
  logCacheMessage(`Caching ${filename}`, parsingCacheOptions)
  void localforage.setItem<CachedParseResult>(cacheKey, {
    [content]: result,
  })
}

export function getPrintAndReparseCodeResult(
  filename: string,
  filePathMappings: FilePathMappings,
  parseSuccess: ParseSuccess,
  stripUIDs: boolean,
  versionNumber: number,
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
    versionNumber,
    alreadyExistingUIDs,
    applySteganography,
    parseCacheOptions,
  )
  return createPrintAndReparseResult(filename, parseResult.parseResult, versionNumber, printedCode)
}
