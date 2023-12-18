/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import '../../../../resources/editor/css/liveblocks/react-comments/styles.css'
import '../../../../resources/editor/css/liveblocks/react-comments/dark/attributes.css'
import React from 'react'
import {
  Button,
  FlexColumn,
  FlexRow,
  Icn,
  InspectorSubsectionHeader,
  Tooltip,
  useColorTheme,
} from '../../../uuiui'
import { stopPropagation } from '../common/inspector-utils'
import { useStorage, type ThreadMetadata } from '../../../../liveblocks.config'
import type { ThreadData } from '@liveblocks/client'
import { useDispatch } from '../../editor/store/dispatch-context'
import { canvasRectangle } from '../../../core/shared/math-utils'
import {
  scrollToPosition,
  setShowResolvedThreads,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes, isCommentMode, isExistingComment } from '../../editor/editor-modes'
import { CommentWrapper, MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { useRemixNavigationContext } from '../../canvas/remix/utopia-remix-root-component'
import {
  useUnresolvedThreads,
  useResolveThread,
  useResolvedThreads,
  useCanvasLocationOfThread,
  getCollaboratorById,
  useMyThreadReadStatus,
} from '../../../core/commenting/comment-hooks'
import { Substores, useEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'
import { unless, when } from '../../../utils/react-conditionals'
import { openCommentThreadActions } from '../../../core/shared/multiplayer'
import { getRemixLocationLabel } from '../../canvas/remix/remix-utils'
import type { RestOfEditorState } from '../../editor/store/store-hook-substore-types'
import { getCurrentTheme } from '../../editor/store/editor-state'

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
  const colorTheme = useColorTheme()

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
        activeThreads.length === 0,
        <div style={{ padding: '0px 8px', color: colorTheme.fg6.value }}>
          No active comment threads.
        </div>,
      )}
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
  const ref = React.useRef<HTMLDivElement>(null)

  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const { remixLocationRoute } = thread.metadata

  const { location, scene: commentScene } = useCanvasLocationOfThread(thread)

  const remixState = useRemixNavigationContext(commentScene)

  const isOnAnotherRoute =
    remixLocationRoute != null && remixLocationRoute !== remixState?.location.pathname

  const onClick = React.useCallback(() => {
    if (isOnAnotherRoute) {
      if (remixState == null) {
        return
      }
      remixState.navigate(remixLocationRoute)
    }
    const rect = canvasRectangle({ x: location.x, y: location.y, width: 25, height: 25 })
    dispatch([
      ...openCommentThreadActions(thread.id, commentScene),
      scrollToPosition(rect, 'to-center'),
    ])
  }, [
    dispatch,
    isOnAnotherRoute,
    remixLocationRoute,
    remixState,
    location,
    thread.id,
    commentScene,
  ])

  const resolveThread = useResolveThread()

  const onResolveThread = React.useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      resolveThread(thread)
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
    },
    [resolveThread, dispatch, thread],
  )

  const readByMe = useMyThreadReadStatus(thread)

  const collabs = useStorage((storage) => storage.collaborators)

  const isSelected = useIsSelectedAndScrollToThread(ref, thread.id)

  const theme = useEditorState(
    Substores.userState,
    (store) => getCurrentTheme(store.userState),
    'ThreadPreview theme',
  )

  const comment = thread.comments[0]
  if (comment == null) {
    return null
  }

  const repliesCount = thread.comments.length - 1

  const remixLocationRouteLabel = getRemixLocationLabel(remixLocationRoute)

  const user = getCollaboratorById(collabs, comment.userId)

  return (
    <div
      ref={ref}
      className={'thread-preview'}
      key={comment.id}
      onClick={onClick}
      css={{
        display: 'flex',
        flexDirection: 'row',
        paddingBottom: 5,
        position: 'relative',
        '&:hover': {
          backgroundColor: isSelected ? colorTheme.primary10.value : colorTheme.bg2.value,
        },
        backgroundColor: isSelected ? colorTheme.primary10.value : 'transparent',
      }}
    >
      <div
        style={{
          height: 46,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          style={{
            width: 8,
            height: 8,
            borderRadius: 8,
            position: 'relative',
            left: 5,
            background: readByMe === 'unread' ? colorTheme.primary.value : 'transparent',
          }}
        />
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          filter: thread.metadata.resolved ? 'grayscale()' : undefined,
        }}
      >
        <CommentWrapper
          data-theme={theme}
          user={user}
          comment={comment}
          showActions={false}
          style={{ backgroundColor: 'transparent' }}
        />
        {when(
          remixLocationRouteLabel != null,
          <div
            style={{
              paddingLeft: 44,
              fontSize: 9,
              color: colorTheme.fg6.value,
            }}
          >
            Route: <span style={{ fontWeight: 500 }}>{remixLocationRouteLabel}</span>
          </div>,
        )}
        {when(
          repliesCount > 0,
          <div
            style={{
              paddingLeft: 44,
              fontSize: 9,
              color: colorTheme.fg6.value,
            }}
          >
            {repliesCount} {repliesCount > 1 ? 'replies' : 'reply'}
          </div>,
        )}
        {unless(repliesCount > 0, <div />)}
      </div>
      <Tooltip title='Resolve' placement='top'>
        <Button
          css={{
            position: 'absolute',
            right: 10,
            top: 10,
          }}
          onClick={onResolveThread}
        >
          <Icn
            category='semantic'
            type={thread?.metadata.resolved ? 'resolved' : 'resolve'}
            width={18}
            height={18}
            color='main'
            css={{
              visibility: thread?.metadata.resolved ? 'visible' : 'hidden',
              '.thread-preview:hover &': {
                visibility: 'visible',
              },
            }}
          />
        </Button>
      </Tooltip>
    </div>
  )
})
ThreadPreview.displayName = 'ThreadPreview'

function useIsSelectedAndScrollToThread(ref: React.RefObject<HTMLDivElement>, threadId: string) {
  const scrollToSelectedCallback = React.useCallback(
    (isSelected: boolean) => {
      if (isSelected && ref.current != null) {
        ref.current.scrollIntoView()
      }
    },
    [ref],
  )

  const isSelectedSelector = React.useCallback(
    (store: RestOfEditorState) => {
      return (
        isCommentMode(store.editor.mode) &&
        store.editor.mode.comment != null &&
        isExistingComment(store.editor.mode.comment) &&
        store.editor.mode.comment.threadId === threadId
      )
    },
    [threadId],
  )

  useSelectorWithCallback(
    Substores.restOfEditor,
    isSelectedSelector,
    scrollToSelectedCallback,
    'useIsSelectedAndScrollToThread isSelected',
  )

  return useEditorState(
    Substores.restOfEditor,
    isSelectedSelector,
    'useIsSelectedAndScrollToThread isSelected',
  )
}
