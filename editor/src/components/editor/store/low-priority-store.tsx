import React from 'react'
import { EditorStateContext, LowPriorityStateContext } from './store-hook'

export const LowPriorityStoreProvider = React.memo<React.PropsWithChildren<unknown>>((props) => {
  const lowPriorityStoreCtx = React.useContext(LowPriorityStateContext)
  return (
    <EditorStateContext.Provider
      value={
        lowPriorityStoreCtx == null
          ? null
          : {
              useStore: lowPriorityStoreCtx.useStore,
              useTrackedStore: lowPriorityStoreCtx.useTrackedStore,
            }
      }
    >
      {props.children}
    </EditorStateContext.Provider>
  )
})
