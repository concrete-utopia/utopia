import React from 'react'
import { useEditorState, Substores } from '../editor/store/store-hook'
import { InspectorPostActionMenu } from '../canvas/controls/select-mode/post-action-menu'

export const PostActionMenuSection = React.memo(() => {
  const postActionSessionInProgress = useEditorState(
    Substores.postActionInteractionSession,
    (store) => store.postActionInteractionSession != null,
    'PostActionMenuSection postActionSessionInProgress',
  )

  if (!postActionSessionInProgress) {
    return null
  }

  return <InspectorPostActionMenu />
})
