import * as React from 'react'
import { triggerForkProject } from './persistence'
import { useRefEditorState } from './store/store-hook'

export function useTriggerForkProject(): () => void {
  const storeRef = useRefEditorState((store) => store)
  return React.useCallback(async () => {
    const store = storeRef.current
    triggerForkProject(store.dispatch, store.editor, store.workers, store.userState.loginState)
  }, [storeRef])
}
