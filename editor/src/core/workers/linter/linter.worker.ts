import { OutgoingLinterWorkerMessage, handleMessage } from './linter-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingLinterWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  handleMessage(event.data, sendMessageWebWorker)
})
