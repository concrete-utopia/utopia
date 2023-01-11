import React from 'react'
import {
  EditorStateContext,
  LowPriorityStateContext,
  OriginalMainEditorStateContext,
} from './store-hook'

export const LowPriorityStoreProvider = React.memo<React.PropsWithChildren<unknown>>((props) => {
  const lowPriorityStore = React.useContext(LowPriorityStateContext)?.useStore
  if (lowPriorityStore == null) {
    throw new Error('LowPriorityStateContext is missing a context provider!')
  }
  return (
    <EditorStateContext.Provider
      value={lowPriorityStore == null ? null : { useStore: lowPriorityStore }}
    >
      {props.children}
    </EditorStateContext.Provider>
  )
})

export const MainEditorStoreProvider = React.memo<React.PropsWithChildren<unknown>>((props) => {
  const mainEditorStore = React.useContext(OriginalMainEditorStateContext)?.useStore
  if (mainEditorStore == null) {
    throw new Error('OriginalMainEditorStateContext is missing a context provider!')
  }
  return (
    <EditorStateContext.Provider value={{ useStore: mainEditorStore }}>
      {props.children}
    </EditorStateContext.Provider>
  )
})
