import React from 'react'
import type { EditorStorePatched } from './editor-state'
import { StoreApi, EqualityChecker, UseBoundStore, Mutate } from 'zustand'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { PERFORMANCE_MARKS_ALLOWED } from '../../../common/env-vars'

type StateSelector<T, U> = (state: T) => U

export const SelectorTimings: {
  current: { [selectorName: string]: { accumulatedTime: number; calledNumberOfTimes: number } }
} = { current: {} }

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
  selector: StateSelector<EditorStorePatched, U>,
  selectorName: string,
  equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
): U => {
  const context = React.useContext(EditorStateContext)

  const wrappedSelector = useWrapSelectorInPerformanceMeasureBlock(selector, selectorName)

  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }
  return context.useStore(wrappedSelector, equalityFn as EqualityChecker<U>)
}

function useWrapSelectorInPerformanceMeasureBlock<U>(
  selector: StateSelector<EditorStorePatched, U>,
  selectorName: string,
): StateSelector<EditorStorePatched, U> {
  const previousSelectorRef = React.useRef<StateSelector<EditorStorePatched, U>>()
  const previousWrappedSelectorRef = React.useRef<StateSelector<EditorStorePatched, U>>()

  if (selector === previousSelectorRef.current && previousWrappedSelectorRef.current != null) {
    // we alreaedy wrapped this selector
    return previousWrappedSelectorRef.current
  } else {
    // let's create a new wrapped selector
    const wrappedSelector = (state: EditorStorePatched) => {
      const LogSelectorPerformance =
        isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED

      const MeasureSelectors = isFeatureEnabled('Debug mode – Measure Selectors')

      if (LogSelectorPerformance) {
        window.performance.mark('selector_begin')
      }

      const beforeSelector = MeasureSelectors ? performance.now() : 0

      let calledNumberOfTimes = 1

      // Uncomment this to stress-test selectors
      // if (MeasureSelectors) {
      //   for (let index = 0; index < 99; index++) {
      //     calledNumberOfTimes++
      //     selector(state)
      //   }
      // }

      const result = selector(state)
      if (LogSelectorPerformance) {
        window.performance.mark('selector_end')
        window.performance.measure(
          `Zustand Selector ${selectorName}`,
          'selector_begin',
          'selector_end',
        )
      }

      const afterSelector = MeasureSelectors ? performance.now() : 0

      if (MeasureSelectors) {
        const currentValue = SelectorTimings.current[selectorName] ?? {
          accumulatedTime: 0,
          calledNumberOfTimes: 0,
        }
        currentValue.accumulatedTime += afterSelector - beforeSelector
        currentValue.calledNumberOfTimes += calledNumberOfTimes
        SelectorTimings.current[selectorName] = currentValue
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
  selector: StateSelector<EditorStorePatched, U>,
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
  // TODO CONCURRENT MODE: We should avoid mutation during the render phase and follow a pattern similar to
  // https://github.com/pmndrs/zustand/blob/d82e103cc6702ed10a404a587163e42fc3ac1338/src/index.ts#L161
  sliceRef.current = selector(api.getState()) // ensure that callers of this always have the latest data
  if (explainMe) {
    console.info('useRefEditorState: reading editor state', sliceRef.current)
  }
  React.useEffect(() => {
    sliceRef.current = selectorRef.current(api.getState()) // the state might have changed between the render and this Effect being called
    if (explainMe) {
      console.info(
        'useRefEditorState: re-reading editor state in useEffect, just in case it changed since the hook was called',
        sliceRef.current,
      )
    }
    if (explainMe) {
      console.info('useRefEditorState: subscribing to the zustand api')
    }
    const unsubscribe = api.subscribe(
      (store: EditorStorePatched) => selectorRef.current(store),
      (newSlice) => {
        if (newSlice != null) {
          if (explainMe) {
            console.info('useRefEditorState: new state slice arrived', newSlice)
          }
          sliceRef.current = newSlice
        }
      },
      { equalityFn: shallowEqual },
    )
    return function cleanup() {
      if (explainMe) {
        console.info('useRefEditorState: unsubscribing from the zustand api')
      }
      unsubscribe()
    }
  }, [api, explainMe])
  return sliceRef
}

export type UtopiaStoreHook = UseBoundStore<EditorStorePatched>

// This is how to officially type the store with a subscribeWithSelector middleware as of Zustand 3.6.0 https://github.com/pmndrs/zustand#using-subscribe-with-selector
export type UtopiaStoreAPI = Mutate<
  StoreApi<EditorStorePatched>,
  [['zustand/subscribeWithSelector', never]]
>

export type EditorStateContextData = {
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
}

export const EditorStateContext = React.createContext<EditorStateContextData | null>(null)
EditorStateContext.displayName = 'EditorStateContext'
export const CanvasStateContext = React.createContext<EditorStateContextData | null>(null)
CanvasStateContext.displayName = 'CanvasStateContext'
export const LowPriorityStateContext = React.createContext<EditorStateContextData | null>(null)
LowPriorityStateContext.displayName = 'LowPriorityStateContext'

export function useSelectorWithCallback<U>(
  selector: StateSelector<EditorStorePatched, U>,
  callback: (newValue: U) => void,
  equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
  explainMe: boolean = false,
): void {
  const context = React.useContext(EditorStateContext)
  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }
  const api = context.api

  const selectorRef = React.useRef(selector)
  selectorRef.current = selector // the selector is possibly a new function instance every time this hook is called

  const equalityFnRef = React.useRef(equalityFn)
  equalityFnRef.current = equalityFn // the equality function is possibly a new function instance every time this hook is called, but we don't want to re-subscribe because of that

  const callbackRef = React.useRef(callback)
  callbackRef.current = callback // the callback function is possibly a new function instance every time this hook is called, but we don't want to re-subscribe because of that

  const previouslySelectedStateRef = React.useRef<U>(selectorRef.current(api.getState()))

  const innerCallback = React.useCallback<(newSlice: U) => void>(
    (newSlice) => {
      // innerCallback is called by Zustand and also by us, to make sure everything is correct we run our own equality check before calling the user's callback here
      if (!equalityFnRef.current(previouslySelectedStateRef.current, newSlice)) {
        if (explainMe) {
          console.info(
            'selected state has a new value according to the provided equalityFn, notifying callback',
            newSlice,
          )
        }
        callbackRef.current(newSlice)
        previouslySelectedStateRef.current = newSlice
      }
    },
    [explainMe],
  )

  /**
   * When the EditorStorePatched is updated, the api.subscribers are called in order. There's a chance one of our parents will be called sooner than us.
   * To make sure the callback is fired when useSelectorWithCallback is called, we manually run the equality check and call the callback if needed.
   * But then to avoid double-calling the callback, we need to prevent it from running again once zustand gets here
   */

  if (explainMe) {
    console.info('useSelectorWithCallback was executed so we call the callback ourselves')
  }
  innerCallback(selectorRef.current(api.getState())) // TODO investigate if we can use subscribeWithSelector's fireImmediately instead of this hack here

  React.useEffect(() => {
    if (explainMe) {
      console.info('subscribing to the api')
    }
    const unsubscribe = api.subscribe(
      (store: EditorStorePatched) => selectorRef.current(store),
      (newSlice) => {
        if (explainMe) {
          console.info('the Zustand api.subscribe is calling our callback')
        }
        innerCallback(newSlice)
      },
      { equalityFn: (oldValue: any, newValue: any) => equalityFnRef.current(oldValue, newValue) },
    )
    return function cleanup() {
      if (explainMe) {
        console.info('unsubscribing from the api')
      }
      unsubscribe()
    }
  }, [api, innerCallback, explainMe])
}
