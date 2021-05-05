import type { EditorAction } from '../../components/editor/action-types'
import type { EditorStore } from '../../components/editor/store/editor-state'

interface Connection {
  subscribe: (listener: (message: { type: string; state: string }) => void) => () => void // adds a change listener. It will be called any time an action is dispatched from the monitor. Returns a function to unsubscribe the current listener.
  unsubscribe: () => void // unsubscribes all listeners.
  send: (action: string, state: any) => void // sends a new action and state manually to be shown on the monitor. If action is null then we suppose we send liftedState.
  init: (state: any) => void // sends the initial state to the monitor.
  error: (message: string) => void // sends the error message to be shown in the extension's monitor.
}

function connectDevToolsExtension(): Connection | null {
  if (
    window != null &&
    (window as any).__REDUX_DEVTOOLS_EXTENSION__ != null &&
    (window as any).__REDUX_DEVTOOLS_EXTENSION__.connect != null
  ) {
    return (window as any).__REDUX_DEVTOOLS_EXTENSION__.connect()
  } else {
    return null
  }
}

const maybeDevTools = connectDevToolsExtension()

const ActionsToOmit: Array<EditorAction['action']> = ['UPDATE_PREVIEW_CONNECTED']

export function updateReduxDevtools(actions: Array<EditorAction>, newStore: EditorStore): void {
  if (maybeDevTools != null) {
    // filter out the actions we are not interested in
    if (!actions.some((a) => ActionsToOmit.includes(a.action))) {
      maybeDevTools.send(actions.map((action) => action.action).join(' '), newStore)
    }
  }
}

export function reduxDevtoolsSendInitialState(newStore: EditorStore): void {
  if (maybeDevTools != null) {
    maybeDevTools.init(newStore)
  }
}
