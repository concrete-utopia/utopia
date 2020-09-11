import { OutgoingValuesWorkerMessage, handleMessage } from './values-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingValuesWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  handleMessage(event.data, sendMessageWebWorker)
})
