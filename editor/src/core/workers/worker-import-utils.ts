import { BASE_URL } from '../../common/env-vars'
import { BranchNameParameterKey, getEditorBranchNameFromURL } from '../../utils/branches'

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

export function createTsWorker(): Worker {
  const branchName = getEditorBranchNameFromURL()
  const baseUrl =
    branchName == null
      ? WORKER_BASE_URL
      : `${WORKER_BASE_URL}?${BranchNameParameterKey}=${branchName}`
  const oldPublicPath = __webpack_public_path__
  __webpack_public_path__ = baseUrl
  const worker = new Worker(new URL('./ts/ts.worker.ts', import.meta.url))
  __webpack_public_path__ = oldPublicPath
  return worker
}

export function createParserPrinterWorker(): Worker {
  const branchName = getEditorBranchNameFromURL()
  const baseUrl =
    branchName == null
      ? WORKER_BASE_URL
      : `${WORKER_BASE_URL}?${BranchNameParameterKey}=${branchName}`
  const oldPublicPath = __webpack_public_path__
  __webpack_public_path__ = baseUrl
  const worker = new Worker(new URL('./parser-printer/parser-printer.worker.ts', import.meta.url))
  __webpack_public_path__ = oldPublicPath
  return worker
}

export function createLinterWorker(): Worker {
  const branchName = getEditorBranchNameFromURL()
  const baseUrl =
    branchName == null
      ? WORKER_BASE_URL
      : `${WORKER_BASE_URL}?${BranchNameParameterKey}=${branchName}`
  const oldPublicPath = __webpack_public_path__
  __webpack_public_path__ = baseUrl
  const worker = new Worker(new URL('./linter/linter.worker.ts', import.meta.url))
  __webpack_public_path__ = oldPublicPath
  return worker
}

export function createWatchdogWorker(): Worker {
  const branchName = getEditorBranchNameFromURL()
  const baseUrl =
    branchName == null
      ? WORKER_BASE_URL
      : `${WORKER_BASE_URL}?${BranchNameParameterKey}=${branchName}`
  const oldPublicPath = __webpack_public_path__
  __webpack_public_path__ = baseUrl
  const worker = new Worker(new URL('./watchdog.worker.ts', import.meta.url))
  __webpack_public_path__ = oldPublicPath
  return worker
}
