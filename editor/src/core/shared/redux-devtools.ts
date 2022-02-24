import type { EditorAction } from '../../components/editor/action-types'
import type { EditorStoreFull } from '../../components/editor/store/editor-state'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { pluck } from './array-utils'
import * as EP from './element-path'
import { ElementInstanceMetadata, ElementInstanceMetadataMap } from './element-template'
import { objectMap } from './object-utils'

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
      sendActionUpdates = !(message.payload.status as boolean)
    }
  }
})

const ActionsToOmit: Array<EditorAction['action']> = ['UPDATE_PREVIEW_CONNECTED', 'LOAD']
const ActionsWithPayload: Array<EditorAction['action']> = []

let lastDispatchedStore: SanitizedState

const PlaceholderMessage = '<<SANITIZED_FROM_DEVTOOLS>>'

type RecursivePartial<T> = {
  [P in keyof T]?: T[P] extends (infer U)[]
    ? RecursivePartial<U>[]
    : T[P] extends object
    ? RecursivePartial<T[P]>
    : T[P]
}

function simplifiedMetadata(elementMetadata: Partial<ElementInstanceMetadata>) {
  return {
    globalFrame: elementMetadata.globalFrame,
  }
}

function simplifiedMetadataMap(metadata: ElementInstanceMetadataMap) {
  const sanitizedSpyData = objectMap((elementMetadata, key) => {
    return simplifiedMetadata(elementMetadata)
  }, metadata)
  return sanitizedSpyData
}

type SanitizedState = ReturnType<typeof sanitizeLoggedState>
function sanitizeLoggedState(store: EditorStoreFull): RecursivePartial<EditorStoreFull> {
  return {
    patchedEditor: {
      jsxMetadata: simplifiedMetadataMap(store.patchedEditor.jsxMetadata) as any,
    },
  }
}

export function reduxDevtoolsSendActions(
  actions: Array<Array<EditorAction>>,
  newStore: EditorStoreFull,
): void {
  if (
    maybeDevTools != null &&
    sendActionUpdates &&
    isFeatureEnabled('Debug mode – Redux Devtools')
  ) {
    // filter out the actions we are not interested in
    const filteredActions = actions
      .flat()
      .filter((action) => !ActionsToOmit.includes(action.action))
      .map((action) =>
        ActionsWithPayload.includes(action.action) ? action : { action: action.action },
      )
    if (filteredActions.length > 0) {
      const sanitizedStore = sanitizeLoggedState(newStore)
      const actionNames = pluck(filteredActions, 'action').join(' ')
      maybeDevTools.send({ type: `⚫️ ${actionNames}`, actions: filteredActions }, sanitizedStore)
      lastDispatchedStore = sanitizedStore
    }
  }
}

export function reduxDevtoolsSendInitialState(newStore: EditorStoreFull): void {
  if (maybeDevTools != null) {
    maybeDevTools.init(newStore)
  }
}

export function reduxDevtoolsLogMessage(message: string, optionalPayload?: any): void {
  if (
    maybeDevTools != null &&
    sendActionUpdates &&
    isFeatureEnabled('Debug mode – Redux Devtools')
  ) {
    maybeDevTools.send({ type: `🟢 ${message}`, payload: optionalPayload }, lastDispatchedStore)
  }
}

export function reduxDevtoolsLogError(message: string, optionalPayload?: any): void {
  if (
    maybeDevTools != null &&
    sendActionUpdates &&
    isFeatureEnabled('Debug mode – Redux Devtools')
  ) {
    maybeDevTools.send({ type: `🔴 ${message}`, payload: optionalPayload }, lastDispatchedStore)
  }
}

export function reduxDevtoolsUpdateState(message: string, newStore: EditorStoreFull): void {
  if (
    maybeDevTools != null &&
    sendActionUpdates &&
    isFeatureEnabled('Debug mode – Redux Devtools')
  ) {
    const sanitizedStore = sanitizeLoggedState(newStore)
    maybeDevTools.send(`🟣 ${message}`, sanitizedStore)
    lastDispatchedStore = sanitizedStore
  }
}
