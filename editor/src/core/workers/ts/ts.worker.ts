import '../../../vite-shims'
import type { OutgoingWorkerMessage } from '../common/worker-types'
import { handleMessage } from './ts-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  handleMessage(event.data, sendMessageWebWorker)
})
