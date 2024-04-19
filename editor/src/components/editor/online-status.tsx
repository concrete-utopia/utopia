import { IS_TEST_ENVIRONMENT, UTOPIA_BACKEND_BASE_URL } from '../../common/env-vars'
import { HEADERS, MODE } from '../../common/server'
import type { EditorDispatch } from './action-types'
import { increaseOnlineStateFailureCount, resetOnlineState } from './actions/action-creators'
import { Substores, useEditorState } from './store/store-hook'

export interface OnlineState {
  runningFailureCount: number
}

export function onlineState(runningFailureCount: number): OnlineState {
  return {
    runningFailureCount: runningFailureCount,
  }
}

export const InitialOnlineState: OnlineState = { runningFailureCount: 0 }

let onlineStateIntervalID: number | null = null

export const FailureLimit = 3

export const OnlineStatusPollingInterval = 5_000

export function useGetOnlineStatus(): boolean {
  const runningFailureCount = useEditorState(
    Substores.onlineState,
    (store) => store.onlineState.runningFailureCount,
    'useGetOnlineStatus runningFailureCount',
  )
  return runningFailureCount < FailureLimit
}

export async function checkOnlineState(): Promise<boolean> {
  if (IS_TEST_ENVIRONMENT) {
    return true
  } else {
    // Perform a HEAD request against the backend to see if we are able to connect to it.
    return fetch(UTOPIA_BACKEND_BASE_URL + 'online-status', {
      method: 'HEAD',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    })
      .then((response) => {
        if (response.ok) {
          // We're able to connect and we get a good status code
          // from the server.
          return true
        } else {
          // We received a failure type response code, which
          // potentially means that our service provider is down or a deploy got mangled.
          return false
        }
      })
      .catch((error) => {
        console.error('Error while fetching online status.', error)
        // There was an error either connecting or while
        // receiving the response.
        return false
      })
  }
}

export function startOnlineStatusPolling(dispatch: EditorDispatch): void {
  // Don't run any of this functionality in tests.
  if (!IS_TEST_ENVIRONMENT) {
    // Trigger this just the once and once started never again.
    if (onlineStateIntervalID == null) {
      const newIntervalID = window.setInterval(async () => {
        const isOnline = await checkOnlineState()
        if (isOnline) {
          dispatch([resetOnlineState()])
        } else {
          dispatch([increaseOnlineStateFailureCount()])
        }
      }, OnlineStatusPollingInterval)

      // Store the interval ID so that we can ensure to only start the interval once.
      onlineStateIntervalID = newIntervalID
    }
  }
}
