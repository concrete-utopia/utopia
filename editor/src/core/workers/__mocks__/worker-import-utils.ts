/**
 * We are mocking core/workers/utils.ts because they use es6 stuff `import.meta.url` for webpack. That is not compatible with Node.js (Jest).
 */

export function createTsWorker() {
  return null
}

export function createParserPrinterWorker() {
  return null
}

export function createLinterWorker() {
  return null
}

export function createWatchdogWorker() {
  return null
}
