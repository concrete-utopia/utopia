import React from 'react'
import { EditorStateContext, LowPriorityStateContext } from './store-hook'

export const LowPriorityStoreProvider = React.memo<React.PropsWithChildren<unknown>>((props) => {
  const lowPriorityStore = React.useContext(LowPriorityStateContext)
  return (
    <EditorStateContext.Provider value={lowPriorityStore == null ? null : lowPriorityStore}>
      {props.children}
    </EditorStateContext.Provider>
  )
})
