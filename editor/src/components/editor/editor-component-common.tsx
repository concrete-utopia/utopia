import React from 'react'
import { EditorAction } from './action-types'
import { EditorStorePatched } from './store/editor-state'
import { useRefEditorState } from './store/store-hook'

type EventHandler<K extends keyof WindowEventMap, R> = (this: Window, event: WindowEventMap[K]) => R

type MouseHandler<R> = EventHandler<'mousedown' | 'mouseup', R>

type MouseHandlerActions = MouseHandler<Array<EditorAction>>

type MouseHandlerUnknown = MouseHandler<unknown>

type KeyboardHandler<R> = EventHandler<'keydown' | 'keyup', R>

type KeyboardHandlerActions = KeyboardHandler<Array<EditorAction>>

type KeyboardHandlerUnknown = KeyboardHandler<unknown>

type EditorStoreRef = { current: EditorStorePatched }

let mouseDownHandlers: Array<MouseHandlerActions> = []
let currentMouseDownEventHandler: MouseHandlerUnknown | null = null

let mouseUpHandlers: Array<MouseHandlerActions> = []
let currentMouseUpEventHandler: MouseHandlerUnknown | null = null

let keyDownHandlers: Array<KeyboardHandlerActions> = []
let currentKeyDownEventHandler: KeyboardHandlerUnknown | null = null

let keyUpHandlers: Array<KeyboardHandlerActions> = []
let currentKeyUpEventHandler: KeyboardHandlerUnknown | null = null

function eventListenerOptionsForEvent(
  eventName: keyof WindowEventMap,
): boolean | EventListenerOptions | undefined {
  switch (eventName) {
    case 'mousedown':
    case 'mouseup':
      return true
    default:
      return undefined
  }
}

// Removes an event handler if it exists.
function removeHandler<K extends keyof WindowEventMap>(
  eventName: K,
  handler: EventHandler<K, unknown> | null,
): void {
  if (handler != null) {
    window.removeEventListener(eventName, handler, eventListenerOptionsForEvent(eventName))
  }
}

// Creates an event handler for a window event from the collection of functions that produce editor actions,
// which dispatches the actions produced by the functions. Assigns that handler to the window event
// and then returns it.
function createHandler<K extends keyof WindowEventMap>(
  editorStoreRef: EditorStoreRef,
  eventName: K,
  handlers: Array<EventHandler<K, Array<EditorAction>>>,
): EventHandler<K, unknown> | null {
  if (handlers.length === 0) {
    return null
  } else {
    const windowEventHandler = (event: WindowEventMap[K]) => {
      const collatedActions = handlers.flatMap((handler) => {
        return handler.bind(window)(event)
      })
      editorStoreRef.current.dispatch(collatedActions, 'everyone')
    }

    window.addEventListener(eventName, windowEventHandler, eventListenerOptionsForEvent(eventName))
    return windowEventHandler
  }
}

// Whenever the global event handler arrays change, this should be invoked to remove
// and recreate the handlers attached to the window.
function recreateEventHandlers(editorStoreRef: EditorStoreRef): void {
  removeHandler('mousedown', currentMouseDownEventHandler)
  removeHandler('mouseup', currentMouseUpEventHandler)
  removeHandler('keydown', currentKeyDownEventHandler)
  removeHandler('keyup', currentKeyUpEventHandler)

  currentMouseDownEventHandler = createHandler(editorStoreRef, 'mousedown', mouseDownHandlers)
  currentMouseUpEventHandler = createHandler(editorStoreRef, 'mouseup', mouseUpHandlers)
  currentKeyDownEventHandler = createHandler(editorStoreRef, 'keydown', keyDownHandlers)
  currentKeyUpEventHandler = createHandler(editorStoreRef, 'keyup', keyUpHandlers)
}

export interface EditorCommonProps {
  mouseDown: MouseHandlerActions
  mouseUp: MouseHandlerActions
  keyDown: KeyboardHandlerActions
  keyUp: KeyboardHandlerActions
}

// Component to be included when functions that produce editor actions from window
// events need to be "stacked" up and only utilise a single instance of the event listener.
export function EditorCommon(props: EditorCommonProps): React.ReactElement | null {
  const editorStoreRef = useRefEditorState((store) => store)
  React.useEffect(() => {
    mouseDownHandlers = [...mouseDownHandlers, props.mouseDown]
    mouseUpHandlers = [...mouseUpHandlers, props.mouseUp]
    keyDownHandlers = [...keyDownHandlers, props.keyDown]
    keyUpHandlers = [...keyUpHandlers, props.keyUp]
    recreateEventHandlers(editorStoreRef)

    return function cleanup() {
      mouseDownHandlers = mouseDownHandlers.filter((handler) => handler !== props.mouseDown)
      mouseUpHandlers = mouseUpHandlers.filter((handler) => handler !== props.mouseUp)
      keyDownHandlers = keyDownHandlers.filter((handler) => handler !== props.keyDown)
      keyUpHandlers = keyUpHandlers.filter((handler) => handler !== props.keyUp)
      recreateEventHandlers(editorStoreRef)
    }
  })

  return null
}
