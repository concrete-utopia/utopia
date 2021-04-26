import * as React from 'react'
import { o } from 'ramda'
import create, { UseStore } from 'zustand'
import { renderHook } from '@testing-library/react-hooks'
import { EditorStateContext, useSelectorWithCallback } from './store-hook'
import { createEditorState, EditorState, EditorStore } from './editor-state'
import { NO_OP } from '../../../core/shared/utils'
import * as TP from '../../../core/shared/template-path'
import { shallowEqual } from '../../../core/shared/equality-utils'

function createEmptyEditorStoreHook() {
  let emptyEditorState = createEditorState(NO_OP)

  const initialEditorStore: EditorStore = {
    editor: emptyEditorState,
    derived: null as any,
    history: null as any,
    userState: null as any,
    workers: null as any,
    dispatch: null as any,
  }

  const storeHook = create<EditorStore>((set) => initialEditorStore)

  return storeHook
}

const ContextProvider: React.FunctionComponent<{
  storeHook: UseStore<EditorStore>
}> = ({ storeHook, children }) => {
  return (
    <EditorStateContext.Provider value={{ api: storeHook, useStore: storeHook }}>
      {children}
    </EditorStateContext.Provider>
  )
}

describe('useSelectorWithCallback', () => {
  it('The callback is not fired on first call', () => {
    const storeHook = createEmptyEditorStoreHook()

    let hookRenders = 0
    let callCount = 0

    const { result } = renderHook<{ storeHook: UseStore<EditorStore> }, void>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          (store) => store.editor.selectedViews,
          (newSelectedViews) => {
            callCount++
          },
        )
      },
      {
        wrapper: ContextProvider,
        initialProps: {
          storeHook: storeHook,
        },
      },
    )

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(0)
  })

  it('The callback is fired if the store changes', () => {
    const storeHook = createEmptyEditorStoreHook()

    let hookRenders = 0
    let callCount = 0

    const { result } = renderHook<{ storeHook: UseStore<EditorStore> }, void>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          (store) => store.editor.selectedViews,
          (newSelectedViews) => {
            callCount++
          },
        )
      },
      {
        wrapper: ContextProvider,
        initialProps: {
          storeHook: storeHook,
        },
      },
    )

    storeHook.setState({
      editor: { selectedViews: [TP.fromString('sb/scene:aaa')] } as EditorState,
    })

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(1)
  })

  it('The callback is fired if the hook is rerendered in a race condition and happens earlier than the zustand subscriber could be notified', () => {
    const storeHook = createEmptyEditorStoreHook()

    let hookRenders = 0
    let callCount = 0
    let hookRendersWhenCallbackWasFired = -1
    let callCountWhenCallbackWasFired = -1

    let rerenderTestHook: () => void

    storeHook.subscribe(
      (newSelectedViews) => {
        if (newSelectedViews != null) {
          rerenderTestHook()
          callCountWhenCallbackWasFired = callCount
          hookRendersWhenCallbackWasFired = hookRenders
          // TODO this is super-baffling. turning this test async and putting done() here and expect()-ing values did not work for some reason
        }
      },
      (store) => store.editor.selectedViews,
    )

    const { result, rerender } = renderHook<{ storeHook: UseStore<EditorStore> }, void>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          (store) => store.editor.selectedViews,
          (newSelectedViews) => {
            callCount++
          },
        )
      },
      {
        wrapper: ContextProvider,
        initialProps: {
          storeHook: storeHook,
        },
      },
    )

    rerenderTestHook = () => {
      rerender({ storeHook: storeHook })
    }

    storeHook.setState({
      editor: { selectedViews: [TP.fromString('sb/scene:aaa')] } as EditorState,
    })

    storeHook.destroy()

    expect(hookRenders).toEqual(2)
    expect(hookRendersWhenCallbackWasFired).toEqual(2)
    expect(callCount).toEqual(1)
    expect(callCountWhenCallbackWasFired).toEqual(1)
  })
})
