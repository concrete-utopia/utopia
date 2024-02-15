import React from 'react'
import { useRefEditorState } from './store/store-hook'
import { persistentModelFromEditorModel } from './store/editor-state'
import { useDispatch } from './store/dispatch-context'
import { setForking } from './actions/action-creators'

export function useTriggerForkProject(): () => void {
  const dispatch = useDispatch()

  const storeRef = useRefEditorState((store) => store)
  return React.useCallback(async () => {
    dispatch([setForking(true)])
    const store = storeRef.current
    store.persistence.fork(store.editor.projectName, persistentModelFromEditorModel(store.editor))
  }, [storeRef, dispatch])
}
