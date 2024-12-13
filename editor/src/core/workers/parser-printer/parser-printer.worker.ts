import '../../../vite-shims'
import type { ClearParseCacheMessage, ParsePrintResultMessage } from '../common/worker-types'
import { handleMessage } from './parser-printer-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: ParsePrintResultMessage | ClearParseCacheMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  void handleMessage(event.data, sendMessageWebWorker)
})
