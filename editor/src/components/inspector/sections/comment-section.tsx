import '@liveblocks/react-comments/styles.css'
import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader } from '../../../uuiui'
import { Comment } from '@liveblocks/react-comments'
import { stopPropagation } from '../common/inspector-utils'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useThreads } from '../../../../liveblocks.config'
import type { ThreadData } from '@liveblocks/client'
import { useDispatch } from '../../editor/store/dispatch-context'
import { canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { scrollToPosition, switchEditorMode } from '../../editor/actions/action-creators'
import { EditorModes } from '../../editor/editor-modes'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'

export const CommentSection = React.memo(() => {
  return (
    <div onKeyDown={stopPropagation} onKeyUp={stopPropagation}>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            height: 42,
          }}
        >
          <span>Comments</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
        <ThreadPreviews />
      </MultiplayerWrapper>
    </div>
  )
})
CommentSection.displayName = 'CommentSection'

const ThreadPreviews = React.memo(() => {
  const { threads } = useThreads()

  return (
    <FlexColumn style={{ gap: 5 }}>
      {threads.map((thread) => (
        <ThreadPreview key={thread.id} thread={thread} />
      ))}
    </FlexColumn>
  )
})
ThreadPreviews.displayName = 'ThreadPreviews'

interface ThreadPreviewProps {
  thread: ThreadData<ThreadMetadata>
}

const ThreadPreview = React.memo(({ thread }: ThreadPreviewProps) => {
  const dispatch = useDispatch()
  const onClick = React.useCallback(() => {
    const point = canvasPoint(thread.metadata)
    const rect = canvasRectangle({ x: point.x, y: point.y, width: 25, height: 25 })
    dispatch([
      switchEditorMode(EditorModes.commentMode(point, 'not-dragging')),
      scrollToPosition(rect, 'to-center'),
    ])
  }, [dispatch, thread.metadata])

  const comment = thread.comments[0]
  if (comment == null) {
    return null
  }
  return (
    <div key={comment.id} onClick={onClick}>
      <Comment comment={comment} showActions={false} />
    </div>
  )
})
ThreadPreview.displayName = 'ThreadPreview'
