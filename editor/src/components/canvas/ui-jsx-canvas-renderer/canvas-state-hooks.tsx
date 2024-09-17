import React from 'react'
import { shallowEqual } from '../../../core/shared/equality-utils'
import type { EditorStorePatched } from '../../editor/store/editor-state'
import type { StateSelector, Substores } from '../../editor/store/store-hook'
import {
  CanvasStateContext,
  useEditorState,
  useRefEditorState,
} from '../../editor/store/store-hook'
import type { StoreKey } from '../../editor/store/store-hook-substore-types'
import { ElementsToRerenderGLOBAL } from '../ui-jsx-canvas'

export function useAllowRerenderOnlyOnAllElements() {
  return React.useCallback(() => ElementsToRerenderGLOBAL.current === 'rerender-all-elements', [])
}

export function useWrappedSelectorToPreventRerender<T, U>(
  selector: StateSelector<T, U>,
  allowRerender: () => boolean,
): StateSelector<T, U> {
  const previousResultRef = React.useRef<U | null>(null)
  return React.useCallback(
    (state: T) => {
      if (allowRerender() || previousResultRef.current == null) {
        // if allowRerender() is false, we will not recompute the selected state, preventing a re-render of the owner component
        previousResultRef.current = selector(state)
      }
      return previousResultRef.current
    },
    [selector, allowRerender],
  )
}

export const useCanvasState = <K extends StoreKey, S extends (typeof Substores)[K], U>(
  storeKey: S,
  allowRerender: () => boolean,
  selector: StateSelector<Parameters<S>[0], U>,
  selectorName: string,
  equalityFn: (oldSlice: U, newSlice: U) => boolean = shallowEqual,
) => {
  return useEditorState(
    storeKey,
    useWrappedSelectorToPreventRerender(selector, allowRerender),
    selectorName,
    equalityFn,
    CanvasStateContext,
  )
}

/**
 * Like useCanvasState, but DOES NOT TRIGGER A RE-RENDER
 *
 * ONLY USE IT IF YOU ARE SURE ABOUT WHAT YOU ARE DOING
 */
export const useRefCanvasState = <U,>(
  selector: StateSelector<EditorStorePatched, U>,
  explainMe = false,
): { readonly current: U } => {
  return useRefEditorState(selector, explainMe, CanvasStateContext)
}
