import * as React from 'react'
import { fork as triggerForkProject } from './persistence/persistence-machine'
import { persistentModelFromEditorModel } from './store/editor-state'
import { useRefEditorState } from './store/store-hook'

export function useTriggerForkProject(): () => void {
  const storeRef = useRefEditorState((store) => store)
  return React.useCallback(async () => {
    const store = storeRef.current
    triggerForkProject()
  }, [storeRef])
}
