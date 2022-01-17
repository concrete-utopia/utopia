import { BASE_URL } from '../../common/env-vars'

/**
 * Webpack 5 only supports a single public path for assets and feeding the Worker url via `import.meta.url`
 * This causes problems when using a CDN, because of same origin restrictions, you are not allowed to load workers from cdns.
 *
 * The solution is to use the webpack-supported global __webpack_public_path__ variable, which updates the value returned by import.meta.url
 *
 * in all of these cases, oldPublicPath is the asset url (the CDN url in deployed editors). we temporarily change it to BASE_URL (which is the url the editor is loaded from)
 * and then after loading the worker, restore it to the CDN url.
 */

const WORKER_BASE_URL = `${BASE_URL}editor/`

import TSWorker from './ts/ts-worker?worker'
import ParserPrinterWorker from './parser-printer/parser-printer.worker?worker'
import LinterWorker from './linter/linter.worker?worker'
import WatchdogWorker from './watchdog.worker?worker'

export function createTsWorker(): Worker {
  // const oldPublicPath = __webpack_public_path__
  // __webpack_public_path__ = WORKER_BASE_URL
  // const worker = new Worker(new URL('./ts/ts.worker.ts', WORKER_BASE_URL))
  // __webpack_public_path__ = oldPublicPath
  // return worker
  return new TSWorker()
}

export function createParserPrinterWorker(): Worker {
  // const oldPublicPath = __webpack_public_path__
  // __webpack_public_path__ = WORKER_BASE_URL
  // const worker = new Worker(new URL('./parser-printer/parser-printer.worker.ts', WORKER_BASE_URL))
  // __webpack_public_path__ = oldPublicPath
  // return worker
  return new ParserPrinterWorker()
}

export function createLinterWorker(): Worker {
  // const oldPublicPath = __webpack_public_path__
  // __webpack_public_path__ = WORKER_BASE_URL
  // const worker = new Worker(new URL('./linter/linter.worker.ts', WORKER_BASE_URL))
  // __webpack_public_path__ = oldPublicPath
  // return worker
  return new LinterWorker()
}

export function createWatchdogWorker(): Worker {
  // const oldPublicPath = __webpack_public_path__
  // __webpack_public_path__ = WORKER_BASE_URL
  // const worker = new Worker(new URL('./watchdog.worker.ts', WORKER_BASE_URL))
  // __webpack_public_path__ = oldPublicPath
  // return worker
  return new WatchdogWorker()
}
