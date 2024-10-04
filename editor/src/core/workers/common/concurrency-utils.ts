import { isFeatureEnabled } from '../../../utils/feature-switches'

// TODO: this will be configurable from the RYO menu
export const PARSE_CONCURRENCY = 3
const PARSER_CONCURRENCY_FEATURE = 'Parallel Parsing'
const LOG_CONCURRENCY_TIMINGS_FEATURE = 'Log Parse Timings'

export function isConcurrencyEnabled() {
  return isFeatureEnabled(PARSER_CONCURRENCY_FEATURE)
}

export function getParserWorkerCount() {
  return isConcurrencyEnabled() ? PARSE_CONCURRENCY : 1
}

export function getParserChunkCount() {
  return isConcurrencyEnabled() ? PARSE_CONCURRENCY : 1
}

// TODO: this will be configurable from the RYO menu
export function isConcurrencyLoggingEnabled() {
  return isFeatureEnabled(LOG_CONCURRENCY_TIMINGS_FEATURE)
}
