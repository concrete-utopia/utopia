import React from 'react'
import create from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import { renderHook } from '@testing-library/react'
import type { UtopiaStoreAPI } from './store-hook'
import {
  createStoresAndState,
  EditorStateContext,
  Substores,
  useSelectorWithCallback,
} from './store-hook'
import type { EditorStorePatched } from './editor-state'
import { createEditorState, EditorState, emptyCollaborativeEditingSupport } from './editor-state'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { notLoggedIn } from '../action-types'
import { emptyProjectServerState } from './project-server-state'
import { InitialOnlineState } from '../online-status'

const initialEditorStore: EditorStorePatched = {
  editor: createEditorState(NO_OP),
  derived: null as any,
  strategyState: null as any,
  history: null as any,
  userState: {
    shortcutConfig: null,
    themeConfig: null,
    loginState: notLoggedIn,
    githubState: {
      authenticated: false,
      gitRepoToLoad: null,
    },
  },
  workers: null as any,
  persistence: null as any,
  saveCountThisSession: 0,
  elementMetadata: null as any,
  postActionInteractionSession: null,
  builtInDependencies: createBuiltInDependenciesList(null),
  storeName: 'editor-store',
  projectServerState: emptyProjectServerState(),
  collaborativeEditingSupport: emptyCollaborativeEditingSupport(),
  onlineState: InitialOnlineState,
}

function createEmptyEditorStoreHook() {
  return createStoresAndState(initialEditorStore)
}

const ContextProvider =
  (storeHook: UtopiaStoreAPI) =>
  ({ children }: { children: React.ReactNode }) => {
    return <EditorStateContext.Provider value={storeHook}>{children}</EditorStateContext.Provider>
  }

describe('useSelectorWithCallback', () => {
  it('The callback is not fired on first call', () => {
    const storeHook = createEmptyEditorStoreHook()

    let hookRenders = 0
    let callCount = 0

    const { result } = renderHook<void, { storeHook: UtopiaStoreAPI }>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          Substores.selectedViews,
          (store) => store.editor.selectedViews,
          (newSelectedViews) => {
            callCount++
          },
          'test selectedViews',
        )
      },
      {
        wrapper: ContextProvider(storeHook),
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

    const { result } = renderHook<void, { storeHook: UtopiaStoreAPI }>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          Substores.selectedViews,
          (store) => store.editor.selectedViews,
          (newSelectedViews) => {
            callCount++
          },
          'test selectedViews',
        )
      },
      {
        wrapper: ContextProvider(storeHook),
        initialProps: {
          storeHook: storeHook,
        },
      },
    )

    storeHook.setState({
      ...initialEditorStore,
      editor: {
        ...storeHook.getState().editor,
        selectedViews: [EP.fromString('sb/scene:aaa')],
      },
    })

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(1)
  })

  it('The callback is fired when the store changes (nullable value)', () => {
    const storeHook = createEmptyEditorStoreHook()

    let hookRenders = 0
    let callCount = 0

    const { result } = renderHook<void, { storeHook: UtopiaStoreAPI }>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          Substores.focusedElement,
          (store) => store.editor.focusedElementPath,
          (newFocusedElementPath) => {
            callCount++
          },
          'test focusedElementPath',
        )
      },
      {
        wrapper: ContextProvider(storeHook),
        initialProps: {
          storeHook: storeHook,
        },
      },
    )

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(0)

    storeHook.setState({
      ...initialEditorStore,
      editor: {
        ...storeHook.getState().editor,
        focusedElementPath: EP.fromString('sb/scene:aaa'),
      },
    })

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(1)

    storeHook.setState({
      ...initialEditorStore,
      editor: { ...storeHook.getState().editor, focusedElementPath: null },
    })

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(2)

    storeHook.setState({
      ...initialEditorStore,
      editor: {
        ...storeHook.getState().editor,
        selectedViews: [EP.fromString('sb/scene:aaa')],
      },
    })

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(2)

    storeHook.setState({
      ...initialEditorStore,
      editor: {
        ...storeHook.getState().editor,
        focusedElementPath: EP.fromString('sb/scene:aaa/bbb'),
      },
    })

    expect(hookRenders).toEqual(1)
    expect(callCount).toEqual(3)
  })

  it('The callback is fired if the hook is rerendered in a race condition and happens earlier than the zustand subscriber could be notified', () => {
    const storeHook = createEmptyEditorStoreHook()

    let hookRenders = 0
    let callCount = 0
    let hookRendersWhenCallbackWasFired = -1
    let callCountWhenCallbackWasFired = -1

    let rerenderTestHook: () => void

    storeHook.stores.fullStore.subscribe(
      (store) => store.editor.selectedViews,
      (newSelectedViews) => {
        if (newSelectedViews != null) {
          rerenderTestHook()
          callCountWhenCallbackWasFired = callCount
          hookRendersWhenCallbackWasFired = hookRenders
          // TODO this is super-baffling. turning this test async and putting done() here and expect()-ing values did not work for some reason
        }
      },
    )

    const { result, rerender } = renderHook<void, { storeHook: UtopiaStoreAPI }>(
      (props) => {
        hookRenders++
        return useSelectorWithCallback(
          Substores.selectedViews,
          (store) => store.editor.selectedViews,
          (newSelectedViews) => {
            callCount++
          },
          'test selectedViews',
        )
      },
      {
        wrapper: ContextProvider(storeHook),
        initialProps: {
          storeHook: storeHook,
        },
      },
    )

    rerenderTestHook = () => {
      rerender({ storeHook: storeHook })
    }

    storeHook.setState({
      ...initialEditorStore,
      editor: {
        ...storeHook.getState().editor,
        selectedViews: [EP.fromString('sb/scene:aaa')],
      },
    })

    Object.values(storeHook.stores).forEach((store) => store.destroy())

    expect(hookRenders).toEqual(2)
    expect(hookRendersWhenCallbackWasFired).toEqual(2)
    expect(callCount).toEqual(1)
    expect(callCountWhenCallbackWasFired).toEqual(1)
  })
})
