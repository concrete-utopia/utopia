import '../../../vite-shims'
import { OutgoingWorkerMessage } from '../common/worker-types'
import { handleMessage } from './asset-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  void handleMessage(event.data, sendMessageWebWorker)
})

ctx.addEventListener('fetch', (event: FetchEvent) => {
  // console.log(event)
  return null
})
