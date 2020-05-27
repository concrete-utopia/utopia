import { OutgoingWorkerMessage, handleMessage } from './ts-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  handleMessage(event.data, sendMessageWebWorker)
})
