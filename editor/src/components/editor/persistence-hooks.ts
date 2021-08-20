import * as React from 'react'
import { triggerForkProject } from './persistence'
import { persistentModelFromEditorModel } from './store/editor-state'
import { useRefEditorState } from './store/store-hook'

export function useTriggerForkProject(): () => void {
  const storeRef = useRefEditorState((store) => store)
  return React.useCallback(async () => {
    const store = storeRef.current
    triggerForkProject(
      store.dispatch,
      persistentModelFromEditorModel(store.editor),
      store.editor.id,
      store.editor.projectName,
      store.userState.loginState,
    )
  }, [storeRef])
}
