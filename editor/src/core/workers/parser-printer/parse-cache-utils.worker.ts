import { isParseableFile } from '../../../core/shared/file-utils'
import { URL_HASH } from '../../../common/env-vars'
import { type ParseCacheOptions } from '../../../core/shared/parse-cache-utils'
import {
  ARBITRARY_CODE_FILE_NAME,
  createParseFileResult,
  type ParseFile,
  type ParseFileResult,
} from '../common/worker-types'
import localforage from 'localforage'
import type { ParsedTextFile } from 'utopia-shared/src/types'

export const CACHE_DB_NAME = 'editor-cache'
export const PARSE_CACHE_STORE_NAME = 'file-parse-cache'
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

function isArbitraryCodeFile(filename: string): boolean {
  return filename === ARBITRARY_CODE_FILE_NAME
}

function stringIdentifiers(filename: string, content: string): (string | object)[] {
  if (isArbitraryCodeFile(filename)) {
    return ['code', [{ content: content }]]
  }
  return [filename]
}

function shouldUseCacheForFile(filename: string, parsingCacheOptions: ParseCacheOptions): boolean {
  return (
    isParseableFile(filename) &&
    (!isArbitraryCodeFile(filename) || parsingCacheOptions.cacheArbitraryCode)
  )
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
  file: ParseFile,
  parsingCacheOptions: ParseCacheOptions,
): Promise<ParseFileResult | null> {
  const { filename, content, versionNumber } = file
  if (!shouldUseCacheForFile(filename, parsingCacheOptions)) {
    return null
  }
  const cacheKey = getCacheKey(filename)
  //check localforage for cache
  const cachedResult = await getParseCacheStore().getItem<CachedParseResult>(cacheKey)
  const cachedResultForContent = cachedResult?.[getCacheIndexKeyWithVersion(content)]
  if (cachedResultForContent != null) {
    logCacheMessage(parsingCacheOptions, 'Cache hit for', ...stringIdentifiers(filename, content))
    return createParseFileResult(filename, cachedResultForContent, versionNumber)
  }
  logCacheMessage(parsingCacheOptions, 'Cache miss for', ...stringIdentifiers(filename, content))
  return null
}

export async function storeParseResultInCache(
  file: ParseFile,
  result: ParsedTextFile,
  parsingCacheOptions: ParseCacheOptions,
): Promise<void> {
  const { filename, content } = file
  if (!shouldUseCacheForFile(filename, parsingCacheOptions)) {
    return
  }
  logCacheMessage(parsingCacheOptions, 'Caching', ...stringIdentifiers(filename, content))
  const cacheKey = getCacheKey(filename)
  if (isArbitraryCodeFile(filename)) {
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

type CachedParseResult = { [fileContent: string]: ParsedTextFile }
