interface IdleDeadline {
  didTimeout: boolean
  timeRemaining: () => DOMHighResTimeStamp
}

interface RequestIdleCallbackOptions {
  timeout: number
}

type RequestIdleCallback = (
  callback: (info: IdleDeadline) => void,
  options?: RequestIdleCallbackOptions,
) => number
// since requestIdleCallback can not be reproduced in JS code, we just use a dummy setTimeout as best effort
// dummy shim implementation from https://developers.google.com/web/updates/2015/08/using-requestidlecallback
const fallbackRequestIdleCallback: RequestIdleCallback = (cb) => {
  const start = Date.now()
  return window.setTimeout(() => {
    cb({
      didTimeout: false,
      timeRemaining: () => {
        return Math.max(0, 50 - (Date.now() - start))
      },
    })
  }, 1)
}
export const requestIdleCallback: RequestIdleCallback =
  (window as any).requestIdleCallback != null
    ? (window as any).requestIdleCallback.bind(window)
    : fallbackRequestIdleCallback

type CancelIdleCallback = (handle: number) => void
const fallbackCancelIdleCallback: CancelIdleCallback = (id) => {
  window.clearTimeout(id)
}
export const cancelIdleCallback: CancelIdleCallback =
  (window as any).cancelIdleCallback != null
    ? (window as any).cancelIdleCallback.bind(window)
    : fallbackCancelIdleCallback
