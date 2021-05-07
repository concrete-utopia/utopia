import { EditorDispatch } from './action-types'
import { updatePreviewConnected } from './actions/action-creators'

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
  let previousPreviewConnected: boolean

  window.clearInterval(intervalId)
  intervalId = window.setInterval(() => {
    const stillConnected =
      lastReportFromPreviewTS > 0 && Date.now() - lastReportFromPreviewTS < timeout
    if (stillConnected !== previousPreviewConnected) {
      dispatch([updatePreviewConnected(stillConnected)], 'everyone')
    }
    previousPreviewConnected = stillConnected
  }, 200)
}
