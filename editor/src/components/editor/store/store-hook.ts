import * as React from 'react'
import { EditorStore } from './editor-state'
import { UseStore, StoreApi } from 'zustand'
import utils from '../../../utils/utils'
import { PRODUCTION_ENV } from '../../../common/env-vars'

type StateSelector<T, U> = (state: T) => U

/**
 * React hooks can only be used in Function Components. useEditorState lets you access the most up to date editor state.
 * useEditorState will only trigger re-render if the return value of your state selector changes (using shallow equality)
 * for the curious, it triggers a re-render internally by using React.useReducer()
 *
 * @param selector
 * your state selector. Use this to pick the relevant properties of the Editor State.
 * The return value of the function is the return value of useEditorState itself.
 * It is a good practice to use object destructure to consume the return value.
 */
export const useEditorState = <U>(
  selector: StateSelector<EditorStore, U>,
  selectorName: string,
  equalityFn = utils.shallowEqual,
): U => {
  const context = React.useContext(EditorStateContext)

  const wrappedSelector = useWrapSelectorInPerformanceMeasureBlock(selector, selectorName)

  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }
  return context.useStore(wrappedSelector, equalityFn)
}

const LogSelectorPerformance = !PRODUCTION_ENV && typeof window.performance.mark === 'function'

function useWrapSelectorInPerformanceMeasureBlock<U>(
  selector: StateSelector<EditorStore, U>,
  selectorName: string,
): StateSelector<EditorStore, U> {
  const previousSelectorRef = React.useRef<StateSelector<EditorStore, U>>()
  const previousWrappedSelectorRef = React.useRef<StateSelector<EditorStore, U>>()

  if (selector === previousSelectorRef.current && previousWrappedSelectorRef.current != null) {
    // we alreaedy wrapped this selector
    return previousWrappedSelectorRef.current
  } else {
    // let's create a new wrapped selector
    const wrappedSelector = (state: EditorStore) => {
      if (LogSelectorPerformance) {
        window.performance.mark('selector_begin')
      }
      const result = selector(state)
      if (LogSelectorPerformance) {
        window.performance.mark('selector_end')
        window.performance.measure(
          `Zustand Selector ${selectorName}`,
          'selector_begin',
          'selector_end',
        )
      }
      return result
    }
    previousSelectorRef.current = selector
    previousWrappedSelectorRef.current = wrappedSelector
    return wrappedSelector
  }
}

/**
 * Like useEditorState, but DOES NOT TRIGGER A RE-RENDER
 *
 * ONLY USE IT IF YOU ARE SURE ABOUT WHAT YOU ARE DOING
 */
export const useRefEditorState = <U>(
  selector: StateSelector<EditorStore, U>,
  explainMe = false,
): { readonly current: U } => {
  const context = React.useContext(EditorStateContext)
  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }
  const api = context.api

  const selectorRef = React.useRef(selector)
  selectorRef.current = selector // the selector is possibly a new function instance every time this hook is called

  const sliceRef = React.useRef(selector(api.getState()))
  if (explainMe) {
    console.info('reading useEditorState', sliceRef.current)
  }
  React.useEffect(() => {
    if (explainMe) {
      console.info('subscribing to the api')
    }
    const unsubscribe = api.subscribe(
      (newSlice) => {
        if (newSlice) {
          if (explainMe) {
            console.info('new slice arrived', newSlice)
          }
          sliceRef.current = newSlice
        }
      },
      (store: EditorStore) => selectorRef.current(store),
      utils.shallowEqual,
    )
    return function cleanup() {
      if (explainMe) {
        console.info('unsubscribing from the api')
      }
      unsubscribe()
    }
  }, [api, explainMe])
  return sliceRef
}

export type UtopiaStoreHook = UseStore<EditorStore>
export type UtopiaStoreAPI = StoreApi<EditorStore>

export type EditorStateContextData = {
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
}

export const EditorStateContext = React.createContext<EditorStateContextData | null>(null)
EditorStateContext.displayName = 'EditorStateContext'
