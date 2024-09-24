import { isFeatureEnabled } from '../../utils/feature-switches'
import type { UtopiaTsWorkers } from '../workers/common/worker-types'

export const CACHE_DB_NAME = 'editor-cache'
export const PARSE_CACHE_STORE_NAME = 'file-parse-cache'

export type ParseCacheOptions = {
  useParsingCache: boolean
  verboseLogCache: boolean
}

function isParseCacheEnabled(): boolean {
  return isFeatureEnabled('Use Parsing Cache')
}

function isVerboseLogCacheEnabled(): boolean {
  // TODO: get from jotai store of "Roll Your Own"
  return isFeatureEnabled('Use Parsing Cache')
}

export function getParseCacheOptions(): ParseCacheOptions {
  return {
    useParsingCache: isParseCacheEnabled(),
    verboseLogCache: isVerboseLogCacheEnabled(),
  }
}

export function deleteParseCache(workers: UtopiaTsWorkers): void {
  workers.sendClearParseCacheMessage()
}
