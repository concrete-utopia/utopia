import React from 'react'
import { useProjectEditorLink } from './links'

export function useCopyProjectLinkToClipboard() {
  const projectEditorLink = useProjectEditorLink()
  return React.useCallback(
    (projectId: string) => {
      window.navigator.clipboard.writeText(projectEditorLink(projectId))
      // TODO notification toast
    },
    [projectEditorLink],
  )
}
