import { printCodeOptions, printCode, lintAndParse } from './parser-printer'
import type {
  ClearParseCacheMessage,
  ParseAndPrintOptions,
  ParseFile,
  ParseFileResult,
  ParsePrintFilesRequest,
  ParsePrintResultMessage,
  PrintAndReparseFile,
  PrintAndReparseResult,
} from '../common/worker-types'
import {
  createParseAndPrintOptions,
  createParseFile,
  createParseFileResult,
  createParsePrintFailedMessage,
  createParsePrintFilesResult,
  createPrintAndReparseResult,
} from '../common/worker-types'
import {
  clearParseCache,
  getParseResultFromCache,
  storeParseResultInCache,
} from './parse-cache-utils.worker'

type ParseOrPrintResults = (ParseFileResult | PrintAndReparseResult)[]

export async function handleMessage(
  workerMessage: ParsePrintFilesRequest | ClearParseCacheMessage,
  sendMessage: (content: ParsePrintResultMessage) => void,
): Promise<void> {
  switch (workerMessage.type) {
    case 'parseprintfiles': {
      try {
        const alreadyExistingUIDs_MUTABLE: Set<string> = new Set(workerMessage.alreadyExistingUIDs)
        const parseOptions = createParseAndPrintOptions(
          workerMessage.filePathMappings,
          alreadyExistingUIDs_MUTABLE,
          workerMessage.applySteganography,
          workerMessage.parsingCacheOptions,
        )
        const { useParsingCache } = parseOptions.parsingCacheOptions
        const fileParseResults = workerMessage.files.map((file) => {
          switch (file.type) {
            case 'parsefile':
              return useParsingCache
                ? getParseFileResultFromCache(file, parseOptions)
                : getParseFileResult(file, parseOptions)
            case 'printandreparsefile':
              return getPrintAndReparseCodeResult(file, parseOptions)
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

async function getParseFileResultFromCache(
  file: ParseFile,
  parseOptions: ParseAndPrintOptions,
): Promise<ParseFileResult> {
  const cachedResult = await getParseResultFromCache(file, parseOptions.parsingCacheOptions)
  if (cachedResult != null) {
    return cachedResult
  }
  // if not in cache, parse the file as usual
  return getParseFileResult(file, parseOptions)
}

export function getParseFileResult(
  file: ParseFile,
  parseOptions: ParseAndPrintOptions,
): ParseFileResult {
  const parseResult = lintAndParse(
    file.filename,
    parseOptions.filePathMappings,
    file.content,
    file.previousParsed,
    parseOptions.alreadyExistingUIDs_MUTABLE,
    'trim-bounds',
    parseOptions.applySteganography,
  )
  const result = createParseFileResult(file.filename, parseResult, file.versionNumber)
  if (
    result.parseResult.type === 'PARSE_SUCCESS' &&
    parseOptions.parsingCacheOptions.useParsingCache
  ) {
    // non blocking cache write
    void storeParseResultInCache(file, result, parseOptions.parsingCacheOptions)
  }

  return result
}

export function getPrintAndReparseCodeResult(
  file: PrintAndReparseFile,
  parseOptions: ParseAndPrintOptions,
): PrintAndReparseResult {
  const { filename, parseSuccess, stripUIDs, versionNumber } = file
  const printedCode = printCode(
    filename,
    printCodeOptions(false, true, true, stripUIDs),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )

  const parseResult = getParseFileResult(
    createParseFile(filename, printedCode, parseSuccess, versionNumber),
    parseOptions,
  )
  return createPrintAndReparseResult(filename, parseResult.parseResult, versionNumber, printedCode)
}

function asSyncResults(
  results: (ParseFileResult | Promise<ParseFileResult> | PrintAndReparseResult)[],
): ParseOrPrintResults {
  return results as ParseOrPrintResults
}
