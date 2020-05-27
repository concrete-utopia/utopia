import { EditorDispatch } from './action-types'
import { updatePreviewConnected } from './actions/actions'

export const InternalPreviewTimeout = 500

let lastReportFromPreviewTS = 0
let timeout = 0
let intervalId: number

export function previewIsAlive(newTimeout: number) {
  timeout = newTimeout
  lastReportFromPreviewTS = Date.now()
}

export function handlePreviewDisconnected() {
  lastReportFromPreviewTS = 0
}

export function startPreviewConnectedMonitoring(dispatch: EditorDispatch) {
  window.clearInterval(intervalId)
  intervalId = window.setInterval(() => {
    const stillConnected =
      lastReportFromPreviewTS > 0 && Date.now() - lastReportFromPreviewTS < timeout
    dispatch([updatePreviewConnected(stillConnected)], 'everyone')
  }, 200)
}
