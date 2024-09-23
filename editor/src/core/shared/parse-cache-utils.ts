import { isFeatureEnabled } from '../../utils/feature-switches'

export type ParseCacheOptions = {
  useParsingCache: boolean
  verboseLogCache: boolean
}

export function isParseCacheEnabled(): boolean {
  return isFeatureEnabled('Use Parsing Cache')
}

export function isVerboseLogCacheEnabled(): boolean {
  return true // isFeatureEnabled('Verbose Log Cache')
}

export function getParseCacheOptions(): ParseCacheOptions {
  return {
    useParsingCache: isParseCacheEnabled(),
    verboseLogCache: isVerboseLogCacheEnabled(),
  }
}
