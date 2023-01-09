/* eslint-disable react-hooks/rules-of-hooks */
import React, { Dispatch } from 'react'
import { EqualityChecker, Mutate, StoreApi, UseBoundStore } from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import create from 'zustand'
import { PERFORMANCE_MARKS_ALLOWED } from '../../../common/env-vars'
import {
  oneLevelNestedEquals,
  shallowEqual,
  twoLevelNestedEquals,
} from '../../../core/shared/equality-utils'
import { objectMap, omit } from '../../../core/shared/object-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import {
  DerivedState,
  EditorStateCanvas,
  EditorStoreFull,
  EditorStorePatched,
  EditorStoreShared,
} from './editor-state'
import { EditorAction, EditorDispatch } from '../action-types'
import {
  CanvasOffsetSubstate,
  CanvasSubstate,
  canvasSubstateKeys,
  DerivedSubstate,
  DispatchSubstate,
  EditorStateWOScrollOffset,
  HighlightedViewsSubstate,
  MetadataSubstate,
  OldEditorState,
  oldEditorStateKeys,
  ProjectContentSubstate,
  restOfStoreKeys,
  SelectedViewsSubstate,
  ThemeSubstate,
} from './store-hook-selectors'
import { editorCursorPositionChanged } from 'utopia-vscode-common'

type StateSelector<T, U> = (state: T) => U

export const SelectorTimings: {
  current: { [selectorName: string]: { accumulatedTime: number; calledNumberOfTimes: number } }
} = { current: {} }

export const SubstoreTimings: {
  current: { [storeKey: string]: { updateTime: number | null; calledNumberOfTimes: number } }
} = { current: {} }

function ensureSubstoreTimingExists(storeKey: string) {
  if (isFeatureEnabled('Debug – Measure Selectors') && SubstoreTimings.current[storeKey] == null) {
    SubstoreTimings.current[storeKey] = {
      updateTime: null,
      calledNumberOfTimes: 0,
    }
  }
}

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
export const useEditorState =
  <K extends StoreKey>(storeKey: K) =>
  <U>(
    selector: StateSelector<Substates[K], U>,
    selectorName: string,
    equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
  ): U => {
    const context = React.useContext(EditorStateContext)

    const wrappedSelector = useWrapSelectorInPerformanceMeasureBlock(
      storeKey,
      selector,
      selectorName,
    )

    if (context == null) {
      throw new Error('useStore is missing from editor context')
    }
    return context.stores[storeKey](wrappedSelector, equalityFn as EqualityChecker<U>)
  }

function useWrapSelectorInPerformanceMeasureBlock<K extends StoreKey, U>(
  storeKey: K,
  selector: StateSelector<Substates[K], U>,
  selectorName: string,
): StateSelector<Substates[K], U> {
  const previousSelectorRef = React.useRef<StateSelector<Substates[K], U>>()
  const previousWrappedSelectorRef = React.useRef<StateSelector<Substates[K], U>>()

  if (selector === previousSelectorRef.current && previousWrappedSelectorRef.current != null) {
    // we alreaedy wrapped this selector
    return previousWrappedSelectorRef.current
  } else {
    // let's create a new wrapped selector
    const wrappedSelector = (state: Substates[K]): U => {
      const LogSelectorPerformance =
        isFeatureEnabled('Debug – Performance Marks (Slow)') && PERFORMANCE_MARKS_ALLOWED

      const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')

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
          preSelectorName: storeKey,
        }
        currentValue.accumulatedTime += afterSelector - beforeSelector
        currentValue.calledNumberOfTimes += calledNumberOfTimes
        SelectorTimings.current[selectorName] = currentValue
        ensureSubstoreTimingExists(storeKey)
        SubstoreTimings.current[storeKey].calledNumberOfTimes += calledNumberOfTimes
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
export const useRefEditorState =
  <K extends StoreKey>(storeKey: K) =>
  <U>(selector: StateSelector<Substates[K], U>, explainMe = false): { readonly current: U } => {
    const context = React.useContext(EditorStateContext)
    if (context == null) {
      throw new Error('useStore is missing from editor context')
    }
    const api = context.stores[storeKey]

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
        (store: Substates[K]) => selectorRef.current(store),
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

// This is how to officially type the store with a subscribeWithSelector middleware as of Zustand 4.1.5 https://github.com/pmndrs/zustand#using-subscribe-with-selector
type Store<S> = UseBoundStore<Mutate<StoreApi<S>, [['zustand/subscribeWithSelector', never]]>>

export type UtopiaStoreAPI = StoresAndSetState

export type EditorStateContextData = StoresAndSetState

export const EditorStateContext = React.createContext<EditorStateContextData | null>(null)
EditorStateContext.displayName = 'EditorStateContext'
export const CanvasStateContext = React.createContext<EditorStateContextData | null>(null)
CanvasStateContext.displayName = 'CanvasStateContext'
export const LowPriorityStateContext = React.createContext<EditorStateContextData | null>(null)
LowPriorityStateContext.displayName = 'LowPriorityStateContext'

export const useSelectorWithCallback =
  <K extends StoreKey>(storeKey: K) =>
  <U>(
    selector: StateSelector<Substates[K], U>,
    callback: (newValue: U) => void,
    equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
    explainMe: boolean = false,
  ): void => {
    const context = React.useContext(EditorStateContext)
    if (context == null) {
      throw new Error('useStore is missing from editor context')
    }
    const api = context.stores[storeKey]

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
        (store: Substates[K]) => selectorRef.current(store),
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

type FullStoreWOScrollOffset = Omit<EditorStorePatched, 'editor'> & EditorStateWOScrollOffset

type Substates = {
  metadata: MetadataSubstate
  selectedViews: SelectedViewsSubstate
  highlightedHoveredViews: HighlightedViewsSubstate
  projectContents: ProjectContentSubstate
  canvas: CanvasSubstate
  canvasOffset: CanvasOffsetSubstate
  derived: { derived: DerivedState }
  oldEditor: { editor: OldEditorState }
  restOfStore: Omit<EditorStorePatched, 'editor' | 'derived'>
  fullOldStore: EditorStorePatched
  originalStore: EditorStorePatched
  dispatch: DispatchSubstate
  theme: ThemeSubstate
}

type StoreKey = keyof Substates

export const SubstateEqualityFns: {
  [key in StoreKey]: (a: Substates[key], b: Substates[key]) => boolean
} = {
  metadata: (a: MetadataSubstate, b: MetadataSubstate): boolean => {
    return (
      a.editor.selectedViews === b.editor.selectedViews &&
      a.editor.spyMetadata === b.editor.spyMetadata &&
      a.editor.domMetadata === b.editor.spyMetadata &&
      a.editor.jsxMetadata === b.editor.jsxMetadata &&
      a.editor.allElementProps === b.editor.allElementProps
      // a.editor._currentAllElementProps_KILLME === b.editor._currentAllElementProps_KILLME // do we need to check this here?
    )
  },
  selectedViews: (a, b) => a.editor.selectedViews === b.editor.selectedViews,
  highlightedHoveredViews: (a: HighlightedViewsSubstate, b: HighlightedViewsSubstate): boolean => {
    return (
      // a.editor.selectedViews === b.editor.selectedViews &&
      a.editor.highlightedViews === b.editor.highlightedViews &&
      a.editor.hoveredViews === b.editor.hoveredViews
    )
  },
  projectContents: (a: ProjectContentSubstate, b: ProjectContentSubstate) => {
    return a.editor.projectContents === b.editor.projectContents
  },
  canvas: (a: CanvasSubstate, b: CanvasSubstate): boolean => {
    return keysEquality(canvasSubstateKeys, a.editor.canvas, b.editor.canvas)
  },
  canvasOffset: (a: CanvasOffsetSubstate, b: CanvasOffsetSubstate) => {
    return keysEquality(
      ['realCanvasOffset', 'roundedCanvasOffset', 'scale'],
      a.editor.canvas,
      b.editor.canvas,
    )
  },
  derived: (a: { derived: DerivedState }, b: { derived: DerivedState }) => {
    return a.derived === b.derived
  },
  oldEditor: (a: { editor: OldEditorState }, b: { editor: OldEditorState }) => {
    return keysEquality(oldEditorStateKeys, a.editor, b.editor)
  },
  restOfStore: (
    a: Omit<EditorStorePatched, 'editor' | 'derived'>,
    b: Omit<EditorStorePatched, 'editor' | 'derived'>,
  ) => {
    return keysEquality(restOfStoreKeys, a, b)
  },
  fullOldStore: (a: EditorStateWOScrollOffset, b: EditorStateWOScrollOffset) => {
    // TODO exclude scroll offset!!!!!!!
    return a === b
  },
  originalStore: (a: EditorStorePatched, b: EditorStorePatched) => a === b,
  dispatch: (a: DispatchSubstate, b: DispatchSubstate) => a.dispatch === b.dispatch,
  theme: (a: ThemeSubstate, b: ThemeSubstate) =>
    a.userState.themeConfig === b.userState.themeConfig,
}

export type UtopiaStores = { [key in StoreKey]: Store<Substates[key]> }
export interface StoresAndSetState {
  stores: UtopiaStores
  setState: (store: EditorStorePatched, dispatchedActions: ReadonlyArray<EditorAction>) => void
  getState: () => EditorStorePatched
}

export const createStoresAndState = (initialEditorStore: EditorStorePatched): StoresAndSetState => {
  // function createSubstates(editorStore: EditorStorePatched): Substates {
  //   return objectMap((picker, key) => {
  //     if (isFeatureEnabled('Selectors Split')) {
  //       return picker(editorStore)
  //     } else {
  //       return editorStore
  //     }
  //   }, SubstatePickers) as Substates // bad type
  // }
  let substores: UtopiaStores = objectMap((_, key) => {
    return create(subscribeWithSelector((set) => initialEditorStore))
  }, SubstateEqualityFns) as UtopiaStores // bad type

  return {
    setState: (
      editorStore: EditorStorePatched,
      dispatchedActions: ReadonlyArray<EditorAction>,
    ): void => {
      const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')
      // console.log('--------------')
      // const substates = createSubstates(editorStore)
      objectMap(<K extends keyof Substates>(substore: UtopiaStores[K], key: K) => {
        // const debug = key === 'restOfStore'
        const beforeStoreUpdate = MeasureSelectors ? performance.now() : 0
        ensureSubstoreTimingExists(key)
        if (!tailoredEqualFunctions(editorStore, substore.getState(), key, dispatchedActions)) {
          // console.log('halal', key)
          substore.setState(editorStore)
          const afterStoreUpdate = MeasureSelectors ? performance.now() : 0
          if (MeasureSelectors) {
            SubstoreTimings.current[key].updateTime =
              (SubstoreTimings.current[key].updateTime ?? 0) + afterStoreUpdate - beforeStoreUpdate
          }
        } else {
          // console.log('bingo', key)
        }
      }, substores)
    },
    getState: (): EditorStorePatched => {
      return substores.originalStore.getState()
    },
    stores: substores,
  }
}

function tailoredEqualFunctions<K extends keyof Substates>(
  editorStore: Substates[K],
  oldEditorStore: Substates[K],
  key: K,
  dispatchedActions: ReadonlyArray<EditorAction>,
) {
  function runTheEqualities() {
    return SubstateEqualityFns[key](oldEditorStore, editorStore)
  }

  if (dispatchedActions[0]?.action === 'SCROLL_CANVAS') {
    runTheEqualities()
    return key === 'canvasOffset' ? false : true
  }
  return runTheEqualities()
}

function keyEquality<T>(key: keyof T, a: T, b: T): boolean {
  return a[key] === b[key]
}
function keysEquality<T>(keys: ReadonlyArray<keyof T>, a: T, b: T): boolean {
  return keys.every((key) => keyEquality(key, a, b))
}
