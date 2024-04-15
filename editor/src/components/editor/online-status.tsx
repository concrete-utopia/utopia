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

export function startOnlineStatusPolling(dispatch: EditorDispatch): void {
  // Don't run any of this functionality in tests.
  if (!IS_TEST_ENVIRONMENT) {
    // Trigger this just the once and once started never again.
    if (onlineStateIntervalID == null) {
      const newIntervalID = window.setInterval(() => {
        // Perform a HEAD request against the backend to see if we are able to react it.
        fetch(UTOPIA_BACKEND_BASE_URL + 'online-status', {
          method: 'HEAD',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
        })
          .then((response) => {
            if (response.ok) {
              // Reset the status, as we're able to connect and we get a good status code
              // from the server.
              dispatch([resetOnlineState()])
            } else {
              // Bump the failure count, as we got a failure type response code, which
              // potentially means that our service provider is down or a deploy got mangled.
              dispatch([increaseOnlineStateFailureCount()])
            }
          })
          .catch((error) => {
            console.error('Error while fetching online status.', error)
            // Bump the failure count as there was an error either connecting or while
            // recieving the response.
            dispatch([increaseOnlineStateFailureCount()])
          })
      }, OnlineStatusPollingInterval)

      // Store the interval ID so that we can ensure to only start the interval once.
      onlineStateIntervalID = newIntervalID
    }
  }
}
