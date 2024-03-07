import urlJoin from 'url-join'
import { useBrowserEnv } from './use-env'
import React from 'react'

export function useProjectEditorLink() {
  const env = useBrowserEnv()
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
