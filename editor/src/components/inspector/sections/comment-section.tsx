import '@liveblocks/react-comments/styles.css'
import React from 'react'
import {
  Button,
  FlexColumn,
  FlexRow,
  InspectorSubsectionHeader,
  useColorTheme,
} from '../../../uuiui'
import { Comment } from '@liveblocks/react-comments'
import { stopPropagation } from '../common/inspector-utils'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import type { ThreadData } from '@liveblocks/client'
import { useDispatch } from '../../editor/store/dispatch-context'
import { canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import {
  scrollToPosition,
  setShowResolvedThreads,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import {
  EditorModes,
  existingComment,
  isCommentMode,
  isExistingComment,
} from '../../editor/editor-modes'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { useAtom } from 'jotai'
import { RemixNavigationAtom } from '../../canvas/remix/utopia-remix-root-component'
import {
  useUnresolvedThreads,
  useIsOnAnotherRemixRoute,
  useResolveThread,
  useResolvedThreads,
} from '../../../core/commenting/comment-hooks'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { unless, when } from '../../../utils/react-conditionals'

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
  const dispatch = useDispatch()
  const { threads: activeThreads } = useUnresolvedThreads()
  const { threads: resolvedThreads } = useResolvedThreads()

  const showResolved = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.showResolvedThreads,
    'ThreadPreviews showResolvedThreads',
  )

  const toggleShowResolved = React.useCallback(() => {
    dispatch([
      setShowResolvedThreads(!showResolved),
      switchEditorMode(EditorModes.selectMode(null, false, 'none')),
    ])
  }, [showResolved, dispatch])

  return (
    <FlexColumn style={{ gap: 5 }}>
      {activeThreads.map((thread) => (
        <ThreadPreview key={thread.id} thread={thread} />
      ))}
      {when(
        resolvedThreads.length > 0,
        <Button
          highlight
          spotlight
          style={{ padding: 10, margin: '10px' }}
          onClick={toggleShowResolved}
        >
          {showResolved ? 'Hide' : 'Show'} resolved threads
        </Button>,
      )}
      {when(
        showResolved,
        resolvedThreads.map((thread) => <ThreadPreview key={thread.id} thread={thread} />),
      )}
    </FlexColumn>
  )
})
ThreadPreviews.displayName = 'ThreadPreviews'

interface ThreadPreviewProps {
  thread: ThreadData<ThreadMetadata>
}

const ThreadPreview = React.memo(({ thread }: ThreadPreviewProps) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const [remixNavigationState] = useAtom(RemixNavigationAtom)

  const { remixLocationRoute } = thread.metadata
  const point = canvasPoint(thread.metadata)

  const isOnAnotherRoute = useIsOnAnotherRemixRoute(remixLocationRoute ?? null)

  const isSelected = useEditorState(
    Substores.restOfEditor,
    (store) =>
      isCommentMode(store.editor.mode) &&
      store.editor.mode.comment != null &&
      isExistingComment(store.editor.mode.comment) &&
      store.editor.mode.comment.threadId === thread.id,
    'ThreadPreview isSelected',
  )

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

  const resolveThread = useResolveThread()

  const onResolveThread = React.useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      resolveThread(thread)
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
    },
    [resolveThread, dispatch, thread],
  )

  const comment = thread.comments[0]
  if (comment == null) {
    return null
  }

  const repliesCount = thread.comments.length - 1
  return (
    <div
      key={comment.id}
      onClick={onClick}
      style={{
        backgroundColor: isSelected
          ? colorTheme.primary10.value
          : thread.metadata.resolved
          ? colorTheme.bg2.value
          : 'transparent',
      }}
    >
      <Comment comment={comment} showActions={false} style={{ backgroundColor: 'transparent' }} />
      <div
        style={{
          paddingBottom: 10,
          paddingLeft: 44,
          paddingRight: 10,
          marginTop: -5,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
        }}
      >
        {when(
          repliesCount > 0,
          <div
            style={{
              fontSize: 9,
              color: colorTheme.fg6.value,
            }}
          >
            {repliesCount} {repliesCount > 1 ? 'replies' : 'reply'}
          </div>,
        )}
        {unless(repliesCount > 0, <div />)}
        <Button highlight spotlight style={{ padding: '0 6px' }} onClick={onResolveThread}>
          {thread.metadata.resolved ? 'Unresolve' : 'Resolve'}
        </Button>
      </div>
    </div>
  )
})
ThreadPreview.displayName = 'ThreadPreview'
