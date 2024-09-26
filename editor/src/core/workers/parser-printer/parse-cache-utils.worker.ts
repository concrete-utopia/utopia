import { URL_HASH } from '../../../common/env-vars'
import { type ParseCacheOptions } from '../../../core/shared/parse-cache-utils'
import type { ParseFileResult } from '../common/worker-types'
import localforage from 'localforage'

export const CACHE_DB_NAME = 'editor-cache'
export const PARSE_CACHE_STORE_NAME = 'file-parse-cache'
const ARBITRARY_CODE_FILE_NAME = 'code.tsx'
const ARBITRARY_CODE_CACHE_KEY_LIMIT = 50

let parseCacheStore: LocalForage | undefined

function getParseCacheStore(): LocalForage {
  if (parseCacheStore === undefined) {
    parseCacheStore = localforage.createInstance({
      name: CACHE_DB_NAME,
      storeName: PARSE_CACHE_STORE_NAME,
    })
  }
  return parseCacheStore
}

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

function shouldSkipCacheForFile(filename: string, parsingCacheOptions: ParseCacheOptions): boolean {
  return filename === ARBITRARY_CODE_FILE_NAME && !parsingCacheOptions.cacheArbitraryCode
}

export function getParseCacheVersion(): string {
  // currently we're using the commit hash which is pretty aggresive cache-busting
  // TODO: consider refining cache-busting strategy to change only on ParsedTextFile shape changes
  return URL_HASH
}

function getCacheIndexKeyWithVersion(content: string): string {
  return `${getParseCacheVersion()}::${content}`
}

export async function getParseResultFromCache(
  filename: string,
  content: string,
  parsingCacheOptions: ParseCacheOptions,
): Promise<ParseFileResult | null> {
  if (shouldSkipCacheForFile(filename, parsingCacheOptions)) {
    return null
  }
  const cacheKey = getCacheKey(filename)
  //check localforage for cache
  const cachedResult = await getParseCacheStore().getItem<CachedParseResult>(cacheKey)
  const cachedResultForContent = cachedResult?.[getCacheIndexKeyWithVersion(content)]
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
): Promise<void> {
  if (shouldSkipCacheForFile(filename, parsingCacheOptions)) {
    return
  }
  logCacheMessage(parsingCacheOptions, 'Caching', ...stringIdentifiers(filename, content))
  const cacheKey = getCacheKey(filename)
  if (filename === ARBITRARY_CODE_FILE_NAME) {
    // for the special filename 'code.tsx', we store multiple contents, so we need to read it first
    const cachedResult = (await getParseCacheStore().getItem<CachedParseResult>(cacheKey)) ?? {}
    // limit the arbitrary code cache keys size
    // TODO: we can use an LRU cache here, but for now this is good enough
    if (Object.keys(cachedResult).length >= ARBITRARY_CODE_CACHE_KEY_LIMIT) {
      const oldestKey = Object.keys(cachedResult)[0]
      delete cachedResult[oldestKey]
    }
    void getParseCacheStore().setItem<CachedParseResult>(cacheKey, {
      ...cachedResult,
      [getCacheIndexKeyWithVersion(content)]: result,
    })
  } else {
    void getParseCacheStore().setItem<CachedParseResult>(cacheKey, {
      [getCacheIndexKeyWithVersion(content)]: result,
    })
  }
}

export async function clearParseCache(parsingCacheOptions: ParseCacheOptions) {
  logCacheMessage(parsingCacheOptions, 'Clearing cache')
  await getParseCacheStore().dropInstance({
    name: CACHE_DB_NAME,
    storeName: PARSE_CACHE_STORE_NAME,
  })
}

type CachedParseResult = { [fileContent: string]: ParseFileResult }
