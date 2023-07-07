import type { OutgoingWatchdogWorkerMessage } from './watchdog-worker'
import { handleMessage } from './watchdog-worker'

const ctx: Worker = self as any

function sendMessageWebWorker(content: OutgoingWatchdogWorkerMessage) {
  ctx.postMessage(content)
}

ctx.addEventListener('message', (event: MessageEvent) => {
  void handleMessage(event.data, sendMessageWebWorker)
})
