/* eslint-disable react-hooks/rules-of-hooks */
import React from 'react'
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
  CanvasOffsetSubstate,
  CanvasSubstate,
  DerivedState,
  EditorStateCanvas,
  EditorStateWOScrollOffset,
  EditorStoreFull,
  EditorStorePatched,
  EditorStoreShared,
  MetadataSubstate,
  OldEditorState,
  ProjectContentSubstate,
  SelectedHighlightedViewsSubstate,
} from './editor-state'

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
  selectedHighlightedViews: SelectedHighlightedViewsSubstate
  projectContents: ProjectContentSubstate
  canvas: CanvasSubstate
  canvasOffset: CanvasOffsetSubstate
  derived: { derived: DerivedState }
  oldEditor: { editor: OldEditorState }
  restOfStore: Omit<EditorStorePatched, 'editor' | 'derived'>
  fullOldStore: EditorStorePatched
  originalStore: EditorStorePatched
}

type StoreKey = keyof Substates

export const SubstatePickers: {
  [key in StoreKey]: (store: EditorStorePatched) => Substates[key]
} = {
  metadata: (store: EditorStorePatched): MetadataSubstate => {
    return {
      editor: {
        selectedViews: store.editor.selectedViews,
        spyMetadata: store.editor.spyMetadata,
        domMetadata: store.editor.domMetadata,
        jsxMetadata: store.editor.jsxMetadata,
        allElementProps: store.editor.allElementProps,
        _currentAllElementProps_KILLME: store.editor._currentAllElementProps_KILLME,
      },
    }
  },
  selectedHighlightedViews: (store: EditorStorePatched): SelectedHighlightedViewsSubstate => {
    return {
      editor: {
        selectedViews: store.editor.selectedViews,
        highlightedViews: store.editor.highlightedViews,
        hoveredViews: store.editor.hoveredViews,
      },
    }
  },
  projectContents: (store: EditorStorePatched) => {
    return {
      editor: {
        projectContents: store.editor.projectContents,
      },
    }
  },
  canvas: (store: EditorStorePatched): CanvasSubstate => {
    return {
      editor: {
        canvas: omit(['realCanvasOffset', 'roundedCanvasOffset'], store.editor.canvas),
      },
    }
  },
  canvasOffset: (store: EditorStorePatched): CanvasOffsetSubstate => {
    return {
      editor: {
        canvas: {
          realCanvasOffset: store.editor.canvas.realCanvasOffset,
          roundedCanvasOffset: store.editor.canvas.roundedCanvasOffset,
          scale: store.editor.canvas.scale,
        },
      },
    }
  },
  derived: (store: EditorStorePatched): { derived: DerivedState } => {
    return { derived: store.derived }
  },
  oldEditor: (store: EditorStorePatched): { editor: OldEditorState } => {
    return {
      editor: {
        id: store.editor.id,
        vscodeBridgeId: store.editor.vscodeBridgeId,
        forkedFromProjectId: store.editor.forkedFromProjectId,
        appID: store.editor.appID,
        projectName: store.editor.projectName,
        projectDescription: store.editor.projectDescription,
        projectVersion: store.editor.projectVersion,
        isLoaded: store.editor.isLoaded,
        branchContents: store.editor.branchContents,
        codeResultCache: store.editor.codeResultCache,
        propertyControlsInfo: store.editor.propertyControlsInfo,
        nodeModules: store.editor.nodeModules,
        hiddenInstances: store.editor.hiddenInstances,
        displayNoneInstances: store.editor.displayNoneInstances,
        warnedInstances: store.editor.warnedInstances,
        lockedElements: store.editor.lockedElements,
        mode: store.editor.mode,
        focusedPanel: store.editor.focusedPanel,
        keysPressed: store.editor.keysPressed,
        mouseButtonsPressed: store.editor.mouseButtonsPressed,
        openPopupId: store.editor.openPopupId,
        toasts: store.editor.toasts,
        cursorStack: store.editor.cursorStack,
        leftMenu: store.editor.leftMenu,
        rightMenu: store.editor.rightMenu,
        interfaceDesigner: store.editor.interfaceDesigner,
        floatingInsertMenu: store.editor.floatingInsertMenu,
        inspector: store.editor.inspector,
        fileBrowser: store.editor.fileBrowser,
        dependencyList: store.editor.dependencyList,
        genericExternalResources: store.editor.genericExternalResources,
        googleFontsResources: store.editor.googleFontsResources,
        projectSettings: store.editor.projectSettings,
        navigator: store.editor.navigator,
        topmenu: store.editor.topmenu,
        preview: store.editor.preview,
        home: store.editor.home,
        lastUsedFont: store.editor.lastUsedFont,
        modal: store.editor.modal,
        localProjectList: store.editor.localProjectList,
        projectList: store.editor.projectList,
        showcaseProjects: store.editor.showcaseProjects,
        codeEditingEnabled: store.editor.codeEditingEnabled,
        codeEditorErrors: store.editor.codeEditorErrors,
        thumbnailLastGenerated: store.editor.thumbnailLastGenerated,
        pasteTargetsToIgnore: store.editor.pasteTargetsToIgnore,
        parseOrPrintInFlight: store.editor.parseOrPrintInFlight,
        safeMode: store.editor.safeMode,
        saveError: store.editor.saveError,
        vscodeBridgeReady: store.editor.vscodeBridgeReady,
        vscodeReady: store.editor.vscodeReady,
        focusedElementPath: store.editor.focusedElementPath,
        config: store.editor.config,
        vscodeLoadingScreenVisible: store.editor.vscodeLoadingScreenVisible,
        indexedDBFailed: store.editor.indexedDBFailed,
        forceParseFiles: store.editor.forceParseFiles,
        githubSettings: store.editor.githubSettings,
        imageDragSessionState: store.editor.imageDragSessionState,
        githubOperations: store.editor.githubOperations,
        githubChecksums: store.editor.githubChecksums,
        githubData: store.editor.githubData,
        refreshingDependencies: store.editor.refreshingDependencies,
        assetChecksums: store.editor.assetChecksums,
      },
    }
  },
  restOfStore: (store: EditorStorePatched): Omit<EditorStorePatched, 'editor' | 'derived'> => {
    return omit(
      [
        'editor',
        'derived',
        'unpatchedEditor',
        'patchedEditor',
        'unpatchedDerived',
        'patchedDerived',
      ],
      store as EditorStoreFull & EditorStorePatched,
    )
  },
  fullOldStore: (store: EditorStorePatched): EditorStorePatched => {
    return {
      ...store,
      editor: {
        ...store.editor,
        canvas: omit(
          ['realCanvasOffset', 'roundedCanvasOffset'],
          store.editor.canvas,
        ) as EditorStateCanvas,
      },
    }
  },
  originalStore: (store) => store,
}

export type UtopiaStores = { [key in StoreKey]: Store<Substates[key]> }
export interface StoresAndSetState {
  stores: UtopiaStores
  setState: (store: EditorStorePatched) => void
  getState: () => EditorStorePatched
}

export const createStoresAndState = (initialEditorStore: EditorStorePatched): StoresAndSetState => {
  function createSubstates(editorStore: EditorStorePatched): Substates {
    return objectMap((picker, key) => {
      if (isFeatureEnabled('Selectors Split')) {
        return picker(editorStore)
      } else {
        return editorStore
      }
    }, SubstatePickers) as Substates // bad type
  }
  const initialSubstates = createSubstates(initialEditorStore)
  let substores: UtopiaStores = objectMap((substateInitialValue, key) => {
    return create(subscribeWithSelector((set) => substateInitialValue))
  }, initialSubstates) as UtopiaStores // bad type

  return {
    setState: (editorStore: EditorStorePatched): void => {
      // console.log('--------------')
      const substates = createSubstates(editorStore)
      objectMap(<K extends keyof Substates>(substore: UtopiaStores[K], key: K) => {
        // const debug = key === 'restOfStore'
        if (!twoLevelNestedEquals(substore.getState(), substates[key])) {
          // console.log('halal', key)
          if (key === 'fullOldStore') {
            substore.setState(substates['originalStore'])
          } else {
            substore.setState(substates[key])
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
