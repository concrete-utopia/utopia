import type { ParseCacheOptions } from '../../shared/parse-cache-utils'
import type { ParseFileResult } from '../common/worker-types'
import localforage from 'localforage'

export const CACHE_DB_NAME = 'editor-cache'
export const PARSE_CACHE_STORE_NAME = 'file-parse-cache'
const ARBITRARY_CODE_FILE_NAME = 'code.tsx'

const parseCache = localforage.createInstance({
  name: CACHE_DB_NAME,
  storeName: PARSE_CACHE_STORE_NAME,
})

function getCacheKey(filename: string): string {
  return `PARSE_CACHE::${filename}`
}

function logCacheMessage(parsingCacheOptions: ParseCacheOptions, ...messages: (string | object)[]) {
  if (parsingCacheOptions.verboseLogCache) {
    console.info(`[PARSING CACHE] `, ...messages)
  }
}

function stringIdentifiers(filename: string, content: string): (string | object)[] {
  if (filename === ARBITRARY_CODE_FILE_NAME) {
    return ['code', [{ content: content }]]
  }
  return [filename]
}

export async function getParseResultFromCache(
  filename: string,
  content: string,
  parsingCacheOptions: ParseCacheOptions,
): Promise<ParseFileResult | null> {
  const cacheKey = getCacheKey(filename)
  //check localforage for cache
  const cachedResult = await parseCache.getItem<CachedParseResult>(cacheKey)
  const cachedResultForContent = cachedResult?.[content]
  if (cachedResultForContent?.parseResult?.type === 'PARSE_SUCCESS') {
    logCacheMessage(parsingCacheOptions, 'Cache hit for', ...stringIdentifiers(filename, content))

    return cachedResultForContent
  }
  logCacheMessage(parsingCacheOptions, 'Cache miss for', ...stringIdentifiers(filename, content))
  return null
}

export async function storeParseResultInCache(
  filename: string,
  content: string,
  result: ParseFileResult,
  parsingCacheOptions: ParseCacheOptions,
) {
  logCacheMessage(parsingCacheOptions, 'Caching', ...stringIdentifiers(filename, content))
  const cacheKey = getCacheKey(filename)
  if (filename === ARBITRARY_CODE_FILE_NAME) {
    // for the special filename 'code.tsx', we store multiple contents, so we need to read it first
    const cachedResult = (await parseCache.getItem<CachedParseResult>(cacheKey)) ?? {}
    void parseCache.setItem<CachedParseResult>(cacheKey, {
      ...cachedResult,
      [content]: result,
    })
  } else {
    void parseCache.setItem<CachedParseResult>(cacheKey, {
      [content]: result,
    })
  }
}

export async function clearParseCache(parsingCacheOptions: ParseCacheOptions) {
  logCacheMessage(parsingCacheOptions, 'Clearing cache')
  await parseCache.dropInstance({ name: CACHE_DB_NAME, storeName: PARSE_CACHE_STORE_NAME })
}

type CachedParseResult = { [fileContent: string]: ParseFileResult }
