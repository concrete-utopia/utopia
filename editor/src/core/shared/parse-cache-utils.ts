import { isFeatureEnabled } from '../../utils/feature-switches'
import type { UtopiaTsWorkers } from '../workers/common/worker-types'

export type ParseCacheOptions = {
  useParsingCache: boolean
  verboseLogCache: boolean
  cacheArbitraryCode: boolean
}

function isParseCacheEnabled(): boolean {
  return isFeatureEnabled('Use Parsing Cache')
}

function isVerboseLogCacheEnabled(): boolean {
  return isFeatureEnabled('Debug - Verbose Log Cache')
}

function isArbitraryCodeCacheEnabled(): boolean {
  return isFeatureEnabled('Debug - Arbitrary Code Cache')
}

export function getParseCacheOptions(): ParseCacheOptions {
  return {
    useParsingCache: isParseCacheEnabled(),
    verboseLogCache: isVerboseLogCacheEnabled(),
    cacheArbitraryCode: isArbitraryCodeCacheEnabled(),
  }
}

export function deleteParseCache(workers: UtopiaTsWorkers): void {
  workers.sendClearParseCacheMessage(getParseCacheOptions())
}
