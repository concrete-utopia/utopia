import React from 'react'
import type { EqualityChecker, Mutate, StoreApi, UseBoundStore } from 'zustand'
import create from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { objectMap } from '../../../core/shared/object-utils'
import {
  MultiplayerSubstateKeepDeepEquality,
  NavigatorStateKeepDeepEquality,
  OnlineStateKeepDeepEquality,
  ProjectServerStateKeepDeepEquality,
} from './store-deep-equality-instances'
import type { DerivedState, EditorStorePatched } from './editor-state'
import {
  logAfterStoreUpdate,
  logBeforeStoreUpdate,
  useWrapCallbackInPerformanceMeasureBlock,
  useWrapSelectorInPerformanceMeasureBlock,
} from './store-hook-performance-logging'
import type {
  BuiltInDependenciesSubstate,
  CanvasAndMetadataSubstate,
  CanvasOffsetSubstate,
  CanvasSubstate,
  FocusedElementPathSubstate,
  GithubSubstate,
  HighlightedHoveredViewsSubstate,
  MetadataAndPropertyControlsInfoSubstate,
  MetadataSubstate,
  MultiplayerSubstate,
  NavigatorSubstate,
  OnlineStateSubstate,
  PostActionInteractionSessionSubstate,
  ProjectContentAndMetadataSubstate,
  ProjectContentSubstate,
  ProjectServerStateSubstate,
  PropertyControlsInfoSubstate,
  RestOfEditorState,
  SelectedViewsSubstate,
  StoreKey,
  Substates,
  ThemeSubstate,
  UserStateSubstate,
  VariablesInScopeSubstate,
} from './store-hook-substore-types'
import {
  canvasOffsetSubstateKeys,
  canvasSubstateKeys,
  focusedElementPathSubstateKeys,
  githubSubstateKeys,
  highlightedHoveredViewsSubstateKeys,
  metadataSubstateKeys,
  projectContentsKeys,
  propertyControlsInfoSubstateKeys,
  restOfEditorStateKeys,
  restOfStoreKeys,
  selectedViewsSubstateKeys,
  variablesInScopeSubstateKeys,
} from './store-hook-substore-types'
import { Getter } from '../hook-utils'
import { uniq } from '../../../core/shared/array-utils'

// This is how to officially type the store with a subscribeWithSelector middleware as of Zustand 4.1.5 https://github.com/pmndrs/zustand#using-subscribe-with-selector
type Store<S> = UseBoundStore<Mutate<StoreApi<S>, [['zustand/subscribeWithSelector', never]]>>

export type UtopiaStores = { [key in StoreKey]: Store<Substates[key]> }

export type UtopiaStoreAPI = {
  stores: UtopiaStores
  setState: (store: EditorStorePatched) => void
  getState: () => EditorStorePatched
}

export const OriginalMainEditorStateContext = React.createContext<UtopiaStoreAPI | null>(null)
OriginalMainEditorStateContext.displayName = 'OriginalMainEditorStateContext'
export const EditorStateContext = React.createContext<UtopiaStoreAPI | null>(null)
EditorStateContext.displayName = 'EditorStateContext'
export const CanvasStateContext = React.createContext<UtopiaStoreAPI | null>(null)
CanvasStateContext.displayName = 'CanvasStateContext'
export const LowPriorityStateContext = React.createContext<UtopiaStoreAPI | null>(null)
LowPriorityStateContext.displayName = 'LowPriorityStateContext'

export const createStoresAndState = (initialEditorStore: EditorStorePatched): UtopiaStoreAPI => {
  let latestStoreState: EditorStorePatched = initialEditorStore

  let substores: UtopiaStores = objectMap((_) => {
    return create(subscribeWithSelector((set) => initialEditorStore))
  }, SubstateEqualityFns) as UtopiaStores // bad type

  return {
    setState: (editorStore: EditorStorePatched): void => {
      latestStoreState = editorStore

      objectMap(<K extends keyof Substates>(substore: UtopiaStores[K], key: K) => {
        logBeforeStoreUpdate(key)

        if (!tailoredEqualFunctions(editorStore, substore.getState(), key)) {
          // we call the original Zustand setState for this substore here
          // Notice that we pass in the _entire_ editorStore as value, not a partial! This saves us from having to create partial objects
          substore.setState(editorStore)

          logAfterStoreUpdate(key)
        }
      }, substores)
    },
    getState: (): EditorStorePatched => {
      return latestStoreState
    },
    stores: substores,
  }
}

export type StateSelector<T, U> = (state: T) => U

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
export const useEditorState = <K extends StoreKey, S extends (typeof Substores)[K], U>(
  storeKey_: S,
  selector: StateSelector<Parameters<S>[0], U>,
  selectorName: string,
  equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
): U => {
  const storeKey: K = storeKey_.name as K
  const context = React.useContext(EditorStateContext)

  const wrappedSelector = useWrapSelectorInPerformanceMeasureBlock(storeKey, selector, selectorName)

  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }
  return context.stores[storeKey](wrappedSelector, equalityFn as EqualityChecker<U>)
}

export const useSelectorWithCallback = <K extends StoreKey, S extends (typeof Substores)[K], U>(
  storeKey_: S,
  selector: StateSelector<Parameters<S>[0], U>,
  callback: (newValue: U) => void,
  selectorName_: string,
  equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
  explainMe: boolean = false,
): void => {
  const storeKey: K = storeKey_.name as K
  const selectorName = `${selectorName_} - useEditorStateWithCallback`

  const context = React.useContext(EditorStateContext)
  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }
  const api = context.stores[storeKey]

  const wrappedSelector = useWrapSelectorInPerformanceMeasureBlock(storeKey, selector, selectorName)

  const selectorRef = React.useRef(wrappedSelector)
  selectorRef.current = wrappedSelector // the selector is possibly a new function instance every time this hook is called

  const equalityFnRef = React.useRef(equalityFn)
  equalityFnRef.current = equalityFn // the equality function is possibly a new function instance every time this hook is called, but we don't want to re-subscribe because of that

  const callbackRef = React.useRef(callback)
  callbackRef.current = callback // the callback function is possibly a new function instance every time this hook is called, but we don't want to re-subscribe because of that

  const previouslySelectedStateRef = React.useRef<U>(selectorRef.current(api.getState()))

  const innerCallback = useWrapCallbackInPerformanceMeasureBlock(
    storeKey,
    callbackRef,
    selectorName,
    previouslySelectedStateRef,
    equalityFnRef,
    explainMe,
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
      (newSlice: U) => {
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
  const api = context
  const getterShell = React.useMemo(() => new Getter<U>(), [])
  getterShell.getter = () => selector(api.getState())

  return getterShell
}

export const Substores = {
  metadata: (a: MetadataSubstate, b: MetadataSubstate): boolean => {
    return keysEquality(metadataSubstateKeys, a.editor, b.editor)
  },
  selectedViews: (a: SelectedViewsSubstate, b: SelectedViewsSubstate) =>
    keysEquality(selectedViewsSubstateKeys, a.editor, b.editor),
  focusedElement: (a: FocusedElementPathSubstate, b: FocusedElementPathSubstate) =>
    keysEquality(focusedElementPathSubstateKeys, a.editor, b.editor),
  highlightedHoveredViews: (
    a: HighlightedHoveredViewsSubstate,
    b: HighlightedHoveredViewsSubstate,
  ): boolean => {
    return keysEquality(highlightedHoveredViewsSubstateKeys, a.editor, b.editor)
  },
  projectContents: (a: ProjectContentSubstate, b: ProjectContentSubstate) => {
    return keysEquality(projectContentsKeys, a.editor, b.editor)
  },
  canvas: (a: CanvasSubstate, b: CanvasSubstate): boolean => {
    return keysEquality(canvasSubstateKeys, a.editor.canvas, b.editor.canvas)
  },
  canvasOffset: (a: CanvasOffsetSubstate, b: CanvasOffsetSubstate) => {
    return keysEquality(canvasOffsetSubstateKeys, a.editor.canvas, b.editor.canvas)
  },
  derived: (a: { derived: DerivedState }, b: { derived: DerivedState }) => {
    return a.derived === b.derived
  },
  restOfEditor: (a: RestOfEditorState, b: RestOfEditorState) => {
    return keysEquality(restOfEditorStateKeys, a.editor, b.editor)
  },
  restOfStore: (
    a: Omit<EditorStorePatched, 'editor' | 'derived'>,
    b: Omit<EditorStorePatched, 'editor' | 'derived'>,
  ) => {
    return keysEquality(restOfStoreKeys, a, b)
  },
  /** @deprecated hurts performance, please avoid using it */
  fullStore: (a: EditorStorePatched, b: EditorStorePatched) => {
    return a === b
  },
  theme: (a: ThemeSubstate, b: ThemeSubstate) =>
    a.userState.themeConfig === b.userState.themeConfig,
  github: (a: GithubSubstate, b: GithubSubstate) => {
    return keysEquality(githubSubstateKeys, a.editor, b.editor)
  },
  builtInDependencies: (a: BuiltInDependenciesSubstate, b: BuiltInDependenciesSubstate) => {
    return a.builtInDependencies === b.builtInDependencies
  },
  userState: (a: UserStateSubstate, b: UserStateSubstate) => {
    return a.userState === b.userState
  },
  canvasAndMetadata: (a: CanvasAndMetadataSubstate, b: CanvasAndMetadataSubstate) => {
    return (
      a.editor.jsxMetadata === b.editor.jsxMetadata &&
      keysEquality(canvasSubstateKeys, a.editor.canvas, b.editor.canvas)
    )
  },
  navigator: (a: NavigatorSubstate, b: NavigatorSubstate) => {
    return NavigatorStateKeepDeepEquality(a.editor.navigator, b.editor.navigator).areEqual
  },
  postActionInteractionSession: (
    a: PostActionInteractionSessionSubstate,
    b: PostActionInteractionSessionSubstate,
  ) => {
    return a.postActionInteractionSession === b.postActionInteractionSession
  },
  projectContentsAndMetadata: (
    a: ProjectContentAndMetadataSubstate,
    b: ProjectContentAndMetadataSubstate,
  ) => {
    return keysEquality([...projectContentsKeys, ...metadataSubstateKeys], a.editor, b.editor)
  },
  projectServerState: (a: ProjectServerStateSubstate, b: ProjectServerStateSubstate) => {
    return ProjectServerStateKeepDeepEquality(a.projectServerState, b.projectServerState).areEqual
  },
  userStateAndProjectServerState: (
    a: ProjectServerStateSubstate & UserStateSubstate,
    b: ProjectServerStateSubstate & UserStateSubstate,
  ) => {
    return (
      a.userState === b.userState &&
      ProjectServerStateKeepDeepEquality(a.projectServerState, b.projectServerState).areEqual
    )
  },
  variablesInScope: (a: VariablesInScopeSubstate, b: VariablesInScopeSubstate) => {
    return keysEquality(variablesInScopeSubstateKeys, a.editor, b.editor)
  },
  multiplayer: (a: MultiplayerSubstate, b: MultiplayerSubstate) => {
    return MultiplayerSubstateKeepDeepEquality(a, b).areEqual
  },
  onlineState: (a: OnlineStateSubstate, b: OnlineStateSubstate): boolean => {
    return OnlineStateKeepDeepEquality(a.onlineState, b.onlineState).areEqual
  },
  propertyControlsInfo: (a: PropertyControlsInfoSubstate, b: PropertyControlsInfoSubstate) => {
    return keysEquality(propertyControlsInfoSubstateKeys, a.editor, b.editor)
  },
  metadataAndPropertyControlsInfo: (
    a: MetadataAndPropertyControlsInfoSubstate,
    b: MetadataAndPropertyControlsInfoSubstate,
  ) => {
    return keysEquality(
      uniq([...propertyControlsInfoSubstateKeys, ...metadataSubstateKeys]),
      a.editor,
      b.editor,
    )
  },
} as const

export const SubstateEqualityFns: {
  [key in StoreKey]: (a: Substates[key], b: Substates[key]) => boolean
} = Substores

function tailoredEqualFunctions<K extends keyof Substates>(
  editorStore: Substates[K],
  oldEditorStore: Substates[K],
  key: K,
) {
  return SubstateEqualityFns[key](oldEditorStore, editorStore)
}

function keyEquality<T>(key: keyof T, a: T, b: T): boolean {
  return a[key] === b[key]
}
function keysEquality<T>(keys: ReadonlyArray<keyof T>, a: T, b: T): boolean {
  return keys.every((key) => keyEquality(key, a, b))
}
