import React from 'react'
import create from 'zustand'
import { EditorStoreFull } from './editor-state'
import { EditorStateContext } from './store-hook'

export const UnpatchedEditorStoreProvider = React.memo((props) => {
  const context = React.useContext(EditorStateContext)
  if (context == null) {
    throw new Error('useStore is missing from editor context')
  }

  const useStore = React.useMemo(() => {
    return create<EditorStoreFull>((set, get, api) => context.api.getState())
  }, [context])

  context.api.subscribe((store) => {
    const mangledStore: EditorStoreFull = {
      ...store,
      patchedEditor: store.unpatchedEditor, // WARNING, WE ARE CHEATING HERE
      patchedDerived: store.unpatchedDerived, // WARNING, WE ARE CHEATING HERE
    }
    useStore.setState(mangledStore)
  })

  return (
    <EditorStateContext.Provider value={{ api: useStore, useStore: useStore }}>
      {props.children}
    </EditorStateContext.Provider>
  )
})
