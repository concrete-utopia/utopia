import type { StrategyState } from '../../components/canvas/canvas-strategies/interaction-state'
import type { EditorAction } from '../../components/editor/action-types'
import type { EditorStoreFull, EditorState } from '../../components/editor/store/editor-state'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { mapDropNulls, pluck } from './array-utils'
import * as EP from './element-path'
import type { ElementInstanceMetadata, ElementInstanceMetadataMap } from './element-template'
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

let lastDispatchedStore: SanitizedState

const PlaceholderMessage = '<<SANITIZED_FROM_DEVTOOLS>>'

function simplifiedMetadata(elementMetadata: ElementInstanceMetadata) {
  return {
    elementPath: EP.toString(elementMetadata.elementPath),
    globalFrame: elementMetadata.globalFrame,
    computedStyle: elementMetadata.computedStyle,
  }
}

function simplifiedMetadataMap(metadata: ElementInstanceMetadataMap) {
  const sanitizedSpyData = objectMap((elementMetadata, key) => {
    return simplifiedMetadata(elementMetadata)
  }, metadata)
  return sanitizedSpyData
}

function sanitizeEditor(editor: EditorState) {
  // When you debug something, feel free to add it to the sanitized editor. right now it contains a few keys that I needed.
  // Be careful about what you add: logging too much stuff chokes the redux devtool
  return {
    selectedViews: editor.selectedViews.map(EP.toString) as any, // this is easier for human consumption
    canvas: {
      mountCount: editor.canvas.mountCount,
      domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount,
      canvasContentInvalidateCount: editor.canvas.canvasContentInvalidateCount,
      controls: {
        ...editor.canvas.controls,
      },
      interactionSession: {
        interactionData: editor.canvas.interactionSession?.interactionData,
        latestMetadata: simplifiedMetadataMap(
          editor.canvas.interactionSession?.latestMetadata ?? {},
        ) as any,
      },
    } as Partial<EditorState['canvas']>,
    jsxMetadata: simplifiedMetadataMap(editor.jsxMetadata) as any,
    domMetadata: simplifiedMetadataMap(editor.domMetadata) as any,
    spyMetadata: simplifiedMetadataMap(editor.spyMetadata) as any,
  } as Partial<EditorState>
}

function sanitizeStrategyState(strategyState: StrategyState) {
  return {
    currentStrategy: strategyState.currentStrategy,
    currentStrategyFitness: strategyState.currentStrategyFitness,
    currentStrategyCommands: strategyState.currentStrategyCommands,
  }
}

type SanitizedState = ReturnType<typeof sanitizeLoggedState>
function sanitizeLoggedState(store: EditorStoreFull) {
  return {
    unpatchedEditor: sanitizeEditor(store.unpatchedEditor),
    patchedEditor: sanitizeEditor(store.patchedEditor),
    strategyState: sanitizeStrategyState(store.strategyState),
  }
}

export function reduxDevtoolsSendActions(
  actions: Array<Array<EditorAction>>,
  newStore: EditorStoreFull,
  isTransient: boolean,
): void {
  if (maybeDevTools != null && sendActionUpdates && isFeatureEnabled('Debug – Redux Devtools')) {
    // filter out the actions we are not interested in
    const filteredActions = mapDropNulls((action) => {
      switch (action.action) {
        //Actions to be completely omitted from logging to redux devtools, to avoid noise
        case 'UPDATE_PREVIEW_CONNECTED': {
          return null
        }
        // These actions will be logged with all of their payload. Be careful: large payloads choke the Redux Devtool logging
        case 'SELECT_COMPONENTS':
        case 'CREATE_INTERACTION_SESSION':
        case 'UPDATE_INTERACTION_SESSION':
        case 'UPDATE_DRAG_INTERACTION_DATA':
        case 'CLEAR_INTERACTION_SESSION': {
          return action
        }
        // List custom printers for specific actions here
        case 'SAVE_DOM_REPORT': {
          return {
            action: action.action,
            cachedPaths: action.cachedPaths,
            elementMetadata: simplifiedMetadataMap(action.elementMetadata),
            reconstructedMetadata: simplifiedMetadataMap(action.reconstructedMetadata),
          }
        }
        // By default, we only log the name of the action, and omit the payload
        default:
          return { action: action.action }
      }
    }, actions.flat())

    if (filteredActions.length > 0) {
      const sanitizedStore = sanitizeLoggedState(newStore)
      const actionNames = pluck(filteredActions, 'action').join(' ')
      const colorBall = isTransient ? `⚪️` : `⚫️`
      maybeDevTools.send(
        { type: `${colorBall} ${actionNames}`, actions: filteredActions },
        sanitizedStore,
      )
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
  if (maybeDevTools != null && sendActionUpdates && isFeatureEnabled('Debug – Redux Devtools')) {
    maybeDevTools.send({ type: `🟢 ${message}`, payload: optionalPayload }, lastDispatchedStore)
  }
}

export function reduxDevtoolsLogError(message: string, optionalPayload?: any): void {
  if (maybeDevTools != null && sendActionUpdates && isFeatureEnabled('Debug – Redux Devtools')) {
    maybeDevTools.send({ type: `🔴 ${message}`, payload: optionalPayload }, lastDispatchedStore)
  }
}

export function reduxDevtoolsUpdateState(message: string, newStore: EditorStoreFull): void {
  if (maybeDevTools != null && sendActionUpdates && isFeatureEnabled('Debug – Redux Devtools')) {
    const sanitizedStore = sanitizeLoggedState(newStore)
    maybeDevTools.send(`🟣 ${message}`, sanitizedStore)
    lastDispatchedStore = sanitizedStore
  }
}
