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
import { EditorModes, existingComment } from '../../editor/editor-modes'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { useAtom } from 'jotai'
import { RemixNavigationAtom } from '../../canvas/remix/utopia-remix-root-component'
import { useIsOnAnotherRemixRoute } from '../../../core/commenting/comment-hooks'

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
  const [remixNavigationState] = useAtom(RemixNavigationAtom)

  const { remixLocationRoute } = thread.metadata
  const point = canvasPoint(thread.metadata)

  const isOnAnotherRoute = useIsOnAnotherRemixRoute(remixLocationRoute ?? null)

  const onClick = React.useCallback(() => {
    const rect = canvasRectangle({ x: point.x, y: point.y, width: 25, height: 25 })

    if (isOnAnotherRoute && remixLocationRoute != null) {
      // TODO: after we have scene identifier in the comment metadata we should only navigate the scene with the comment
      Object.keys(remixNavigationState).forEach((scene) => {
        const remixState = remixNavigationState[scene]
        if (remixState == null) {
          return
        }
        remixState.navigate(remixLocationRoute)
      })
    }
    dispatch([
      switchEditorMode(EditorModes.commentMode(existingComment(thread.id), 'not-dragging')),
      scrollToPosition(rect, 'to-center'),
    ])
  }, [dispatch, remixNavigationState, isOnAnotherRoute, remixLocationRoute, point, thread.id])

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
