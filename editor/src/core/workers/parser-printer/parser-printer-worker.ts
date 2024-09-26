import type { ParseSuccess } from '../../shared/project-file-types'
import type { SteganographyMode } from './parser-printer'
import { printCodeOptions, printCode, lintAndParse } from './parser-printer'
import type {
  ClearParseCacheMessage,
  ParseFile,
  ParseFileResult,
  ParsePrintFilesRequest,
  ParsePrintResultMessage,
  PrintAndReparseFile,
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

type ParseOrPrintResults = (ParseFileResult | PrintAndReparseResult)[]

export async function handleMessage(
  workerMessage: ParsePrintFilesRequest | ClearParseCacheMessage,
  sendMessage: (content: ParsePrintResultMessage) => void,
): Promise<void> {
  switch (workerMessage.type) {
    case 'parseprintfiles': {
      try {
        const alreadyExistingUIDs_MUTABLE: Set<string> = new Set(workerMessage.alreadyExistingUIDs)
        const { useParsingCache } = workerMessage.parsingCacheOptions

        const fileParseResults = workerMessage.files.map((file) => {
          switch (file.type) {
            case 'parsefile':
              return useParsingCache
                ? handleParseFileWithCache(file, workerMessage, alreadyExistingUIDs_MUTABLE)
                : handleParseFile(file, workerMessage, alreadyExistingUIDs_MUTABLE)
            case 'printandreparsefile':
              return handlePrintAndReparseFile(file, workerMessage, alreadyExistingUIDs_MUTABLE)
            default:
              const _exhaustiveCheck: never = file
              throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
          }
        })

        // this separation is to make sure that we will Promise.all only if the cache is used
        const results = useParsingCache
          ? await Promise.all(fileParseResults)
          : asSyncResults(fileParseResults)

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

async function handleParseFileWithCache(
  file: ParseFile,
  workerMessage: ParsePrintFilesRequest,
  alreadyExistingUIDs_MUTABLE: Set<string>,
): Promise<ParseFileResult> {
  const cachedResult = await getParseResultFromCache(
    file.filename,
    file.content,
    workerMessage.parsingCacheOptions,
  )
  if (cachedResult != null) {
    return cachedResult
  }

  // if not in cache, parse the file as usual
  return handleParseFile(file, workerMessage, alreadyExistingUIDs_MUTABLE)
}

function handleParseFile(
  file: ParseFile,
  workerMessage: ParsePrintFilesRequest,
  alreadyExistingUIDs_MUTABLE: Set<string>,
): ParseFileResult {
  return getParseFileResult(
    file.filename,
    workerMessage.filePathMappings,
    file.content,
    file.previousParsed,
    file.versionNumber,
    alreadyExistingUIDs_MUTABLE,
    workerMessage.applySteganography,
    workerMessage.parsingCacheOptions,
  )
}

function handlePrintAndReparseFile(
  file: PrintAndReparseFile,
  workerMessage: ParsePrintFilesRequest,
  alreadyExistingUIDs_MUTABLE: Set<string>,
): PrintAndReparseResult {
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

function asSyncResults(
  results: (ParseFileResult | Promise<ParseFileResult> | PrintAndReparseResult)[],
): ParseOrPrintResults {
  return results as ParseOrPrintResults
}
