import '../../../vite-shims'
import type { OutgoingLinterWorkerMessage } from './linter-worker'
import { handleMessage } from './linter-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingLinterWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  handleMessage(event.data, sendMessageWebWorker)
})
