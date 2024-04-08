import React from 'react'
import urlJoin from 'url-join'
import { useAppStore } from '../stores/appStore'

export function useProjectEditorLink() {
  const env = useAppStore((store) => store.env)
  return React.useCallback(
    (projectId: string | null) => {
      if (env?.EDITOR_URL == null) {
        throw new Error('missing editor url')
      }
      return urlJoin(env.EDITOR_URL, 'project', projectId ?? '')
    },
    [env],
  )
}
