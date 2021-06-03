import type { EditorAction } from '../../components/editor/action-types'
import type { EditorStore } from '../../components/editor/store/editor-state'
import { pluck } from './array-utils'

interface Connection {
  subscribe: (listener: (message: { type: string; payload: any }) => void) => () => void // adds a change listener. It will be called any time an action is dispatched from the monitor. Returns a function to unsubscribe the current listener.
  unsubscribe: () => void // unsubscribes all listeners.
  send: (action: any, state: SanitizedState) => void // sends a new action and state manually to be shown on the monitor. If action is null then we suppose we send liftedState.
  init: (state: any) => void // sends the initial state to the monitor.
  error: (message: string) => void // sends the error message to be shown in the extension's monitor.
}

function connectDevToolsExtension(): Connection | null {
  if (
    window != null &&
    (window as any).__REDUX_DEVTOOLS_EXTENSION__ != null &&
    (window as any).__REDUX_DEVTOOLS_EXTENSION__.connect != null
  ) {
    return (window as any).__REDUX_DEVTOOLS_EXTENSION__.connect({
      maxAge: 50, // maximum allowed actions to be stored in the history tree. It's critical for performance
      features: {
        pause: true, // start/pause recording of dispatched actions
        lock: false, // lock/unlock dispatching actions and side effects
        persist: false, // persist states on page reloading
        export: true, // export history of actions in a file
        import: 'custom', // import history of actions from a file
        jump: false, // jump back and forth (time travelling)
        skip: false, // skip (cancel) actions
        reorder: false, // drag and drop actions in the history list
        dispatch: false, // dispatch custom actions or action creators
        test: false, // generate tests for the selected actions
      },
    })
  } else {
    return null
  }
}

const maybeDevTools = connectDevToolsExtension()
let sendActionUpdates = true

// eslint-disable-next-line no-unused-expressions
maybeDevTools?.subscribe((message) => {
  // console.log('message from devtools', message, message.type)
  if (message.type === 'DISPATCH') {
    if (message.payload.type === 'IMPORT_STATE') {
      // this is a very primitive way to enable Redux Devtool to import someone elses exported state.
      // It tells the devtool that this is our new state, essentially lying about it.
      maybeDevTools.send(null, message.payload.nextLiftedState)
      sendActionUpdates = false
    } else if (message.payload.type === 'PAUSE_RECORDING') {
      sendActionUpdates = !message.payload.status
    }
  }
})

const ActionsToOmit: Array<EditorAction['action']> = ['UPDATE_PREVIEW_CONNECTED']

let lastDispatchedStore: SanitizedState

const PlaceholderMessage = '<<SANITIZED_FROM_DEVTOOLS>>'

type SanitizedState = ReturnType<typeof sanitizeLoggedState>
function sanitizeLoggedState(store: EditorStore) {
  return {
    ...store,
    editor: {
      ...store.editor,
      codeResultCache: {
        ...store.editor.codeResultCache,
        cache: PlaceholderMessage,
        requireFn: PlaceholderMessage,
        resolve: PlaceholderMessage,
        evaluationCache: PlaceholderMessage,
      },
      nodeModules: {
        ...store.editor.nodeModules,
        files: PlaceholderMessage,
      },
      canvas: {
        ...store.editor.canvas,
        base64Blobs: PlaceholderMessage,
      },
    },
    history: PlaceholderMessage,
    workers: PlaceholderMessage,
    dispatch: PlaceholderMessage,
  }
}

export function reduxDevtoolsSendActions(
  actions: Array<Array<EditorAction>>,
  newStore: EditorStore,
): void {
  if (maybeDevTools != null && sendActionUpdates) {
    // filter out the actions we are not interested in
    const filteredActions = actions
      .flat()
      .filter((action) => !ActionsToOmit.includes(action.action))
    if (filteredActions.length > 0) {
      const sanitizedStore = sanitizeLoggedState(newStore)
      const actionNames = pluck(filteredActions, 'action').join(' ')
      maybeDevTools.send({ type: `⚫️ ${actionNames}`, actions: filteredActions }, sanitizedStore)
      lastDispatchedStore = sanitizedStore
    }
  }
}

export function reduxDevtoolsSendInitialState(newStore: EditorStore): void {
  if (maybeDevTools != null && sendActionUpdates) {
    maybeDevTools.init(newStore)
  }
}

export function reduxDevtoolsLogMessage(message: string, optionalPayload?: any): void {
  if (maybeDevTools != null && sendActionUpdates) {
    maybeDevTools.send({ type: `🟢 ${message}`, payload: optionalPayload }, lastDispatchedStore)
  }
}

export function reduxDevtoolsLogError(message: string, optionalPayload?: any): void {
  if (maybeDevTools != null && sendActionUpdates) {
    maybeDevTools.send({ type: `🔴 ${message}`, payload: optionalPayload }, lastDispatchedStore)
  }
}

export function reduxDevtoolsUpdateState(message: string, newStore: EditorStore): void {
  if (maybeDevTools != null && sendActionUpdates) {
    const sanitizedStore = sanitizeLoggedState(newStore)
    maybeDevTools.send(`🟣 ${message}`, sanitizedStore)
    lastDispatchedStore = sanitizedStore
  }
}
