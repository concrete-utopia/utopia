import React from 'react'
import { identity } from '../../core/shared/utils'
import { useRefEditorState } from './store/store-hook'

export function useTriggerForkProject(): () => void {
  const storeRef = useRefEditorState(identity)
  return React.useCallback(async () => {
    const store = storeRef.current
    store.persistence.fork()
  }, [storeRef])
}
