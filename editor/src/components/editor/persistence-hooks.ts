import React from 'react'
import { useRefEditorState } from './store/store-hook'

export function useTriggerForkProject(): () => void {
  const storeRef = useRefEditorState('fullOldStore')((store) => store)
  return React.useCallback(async () => {
    const store = storeRef.current
    store.persistence.fork()
  }, [storeRef])
}
