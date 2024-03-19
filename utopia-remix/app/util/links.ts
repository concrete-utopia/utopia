import urlJoin from 'url-join'
import React from 'react'
import { useProjectsStore } from '../store'

export function useProjectEditorLink() {
  const env = useProjectsStore((store) => store.env)
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
