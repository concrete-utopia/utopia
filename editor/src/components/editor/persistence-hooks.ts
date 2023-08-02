import React from 'react'
import { useRefEditorState } from './store/store-hook'
import { persistentModelFromEditorModel } from './store/editor-state'

export function useTriggerForkProject(): () => void {
  const storeRef = useRefEditorState((store) => store)
  return React.useCallback(async () => {
    const store = storeRef.current
    store.persistence.fork(store.editor.projectName, persistentModelFromEditorModel(store.editor))
  }, [storeRef])
}
