import { OutgoingWatchdogWorkerMessage, handleMessage } from './watchdog-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingWatchdogWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  handleMessage(event.data, sendMessageWebWorker)
})
