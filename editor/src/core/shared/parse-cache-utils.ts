import { isFeatureEnabled } from '../../utils/feature-switches'
import type { UtopiaTsWorkers } from '../workers/common/worker-types'

export type ParseCacheOptions = {
  useParsingCache: boolean
  verboseLogCache: boolean
}

function isParseCacheEnabled(): boolean {
  return isFeatureEnabled('Use Parsing Cache')
}

function isVerboseLogCacheEnabled(): boolean {
  return isFeatureEnabled('Verbose Log Cache')
}

export function getParseCacheOptions(): ParseCacheOptions {
  return {
    useParsingCache: isParseCacheEnabled(),
    verboseLogCache: isVerboseLogCacheEnabled(),
  }
}

export function deleteParseCache(workers: UtopiaTsWorkers): void {
  workers.sendClearParseCacheMessage(getParseCacheOptions())
}
