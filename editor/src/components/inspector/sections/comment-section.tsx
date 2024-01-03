/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import '../../../../resources/editor/css/liveblocks/react-comments/styles.css'
import '../../../../resources/editor/css/liveblocks/react-comments/dark/attributes.css'
import React, { useState } from 'react'
import {
  Button,
  CheckboxInput,
  FlexColumn,
  FlexRow,
  Icn,
  InspectorSubsectionHeader,
  Tooltip,
  color,
  useColorTheme,
} from '../../../uuiui'
import { stopPropagation } from '../common/inspector-utils'
import { useStorage, type ThreadMetadata, useThreads } from '../../../../liveblocks.config'
import type { ThreadData } from '@liveblocks/client'
import { useDispatch } from '../../editor/store/dispatch-context'
import { canvasRectangle, rectangleContainsRectangle } from '../../../core/shared/math-utils'
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
  useReadThreads,
} from '../../../core/commenting/comment-hooks'
import { Substores, useEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'
import { when } from '../../../utils/react-conditionals'
import { openCommentThreadActions } from '../../../core/shared/multiplayer'
import { getRemixLocationLabel } from '../../canvas/remix/remix-utils'
import type { RestOfEditorState } from '../../editor/store/store-hook-substore-types'
import { getCurrentTheme } from '../../editor/store/editor-state'
import type { EditorAction } from '../../editor/action-types'
import { canvasPointToWindowPoint } from '../../canvas/dom-lookup'
import { CommentRepliesCounter } from '../../canvas/controls/comment-replies-counter'
import { filter } from 'jszip'

export const CommentSection = React.memo(() => {
  return (
    <div onKeyDown={stopPropagation} onKeyUp={stopPropagation}>
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

  const [filtersOpen, setOpen] = React.useState(true)
  const toggleOpen = React.useCallback(() => {
    setOpen((prevOpen) => !prevOpen)
  }, [setOpen])

  const { threads: activeThreads } = useUnresolvedThreads()
  const { threads: resolvedThreads } = useResolvedThreads()

  const { threads: readThreads } = useReadThreads()

  const allThreadsResolvedLast = [...activeThreads, ...resolvedThreads]
  const { threads: allThreads } = useThreads()

  const unreadThreads = allThreadsResolvedLast.filter((thread) => !readThreads.includes(thread))

  const threads = allThreads

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

  const showRead = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.showResolvedThreads,
    'ThreadPreviews showResolvedThreads',
  )

  const toggleShowRead = React.useCallback(() => {
    dispatch([
      setShowResolvedThreads(!showResolved),
      switchEditorMode(EditorModes.selectMode(null, false, 'none')),
    ])
  }, [showResolved, dispatch])

  return (
    <FlexColumn style={{ gap: 5 }}>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            justifyContent: 'space-between',
            alignItems: 'center',
          }}
        >
          <span>Comments</span>
          <div
            css={{
              width: 20,
              height: 20,
              borderRadius: 3,
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
              '&:hover': {
                backgroundColor: colorTheme.bg3.value,
              },
            }}
          >
            <div
              css={{
                height: 16,
                width: 16,
                borderRadius: 16,
                border: `1px solid ${colorTheme.fg1.value}`,
                cursor: 'pointer',
                display: 'flex',
                flexDirection: 'column',
                justifyContent: 'center',
                alignItems: 'center',
                gap: 1,
              }}
              onClick={toggleOpen}
            >
              <div style={{ width: 6, height: 1, background: colorTheme.fg1.value }} />
              <div style={{ width: 4, height: 1, background: colorTheme.fg1.value }} />
              <div style={{ width: 3, height: 1, background: colorTheme.fg1.value }} />
            </div>
          </div>
        </FlexRow>
      </InspectorSubsectionHeader>
      {when(
        activeThreads.length > 0,
        <FlexColumn
          style={{
            gap: 6,
            overflow: 'hidden',
            padding: filtersOpen ? 8 : 0,
            height: filtersOpen ? 'auto' : 0,
          }}
        >
          <FlexRow>
            <CheckboxInput
              style={{ marginRight: 8 }}
              id='showResolved'
              checked={showResolved}
              onChange={toggleShowResolved}
            />
            <label htmlFor='showResolved'>Sort By Date</label>
          </FlexRow>
          <FlexRow>
            <CheckboxInput
              style={{ marginRight: 8 }}
              id='showResolved'
              checked={showResolved}
              onChange={toggleShowResolved}
            />
            <label htmlFor='showResolved'>Sort By Unread</label>
          </FlexRow>
          {when(
            resolvedThreads.length > 0 || readThreads.length > 0,
            <div style={{ width: '100%', background: colorTheme.bg3.value, height: 1 }} />,
          )}
          {when(
            resolvedThreads.length > 0,
            <FlexRow>
              <CheckboxInput
                style={{ marginRight: 8 }}
                id='showResolved'
                checked={showResolved}
                onChange={toggleShowResolved}
              />
              <label htmlFor='showResolved'>Show Resolved Comments</label>
            </FlexRow>,
          )}
          {when(
            readThreads.length > 0,
            <FlexRow>
              <CheckboxInput
                style={{ marginRight: 8 }}
                id='showRead'
                checked={showRead}
                onChange={toggleShowRead}
              />
              <label htmlFor='showResolved'>Show Read Comments</label>
            </FlexRow>,
          )}
        </FlexColumn>,
      )}
      {when(
        activeThreads.length === 0,
        <div style={{ padding: 8, color: colorTheme.fg6.value }}>No active comment threads.</div>,
      )}
      {activeThreads.map((thread) => (
        <ThreadPreview key={thread.id} thread={thread} />
      ))}
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

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'ThreadPreview canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'ThreadPreview canvasOffset',
  )

  const onClick = React.useCallback(() => {
    if (isOnAnotherRoute) {
      if (remixState == null) {
        return
      }
      remixState.navigate(remixLocationRoute)
    }
    let actions: EditorAction[] = [...openCommentThreadActions(thread.id, commentScene)]

    const canvasDiv = document.getElementById('canvas-root')
    if (canvasDiv != null) {
      const canvasArea = canvasDiv.getBoundingClientRect()
      const visibleAreaTolerance = 200 // px
      const visibleArea = canvasRectangle({
        x: canvasArea.x + visibleAreaTolerance,
        y: canvasArea.y,
        width: canvasArea.width - visibleAreaTolerance * 2,
        height: canvasArea.height,
      })
      const rect = canvasRectangle({ x: location.x, y: location.y, width: 25, height: 25 })
      const windowLocation = canvasPointToWindowPoint(location, canvasScale, canvasOffset)
      const windowRect = canvasRectangle({
        x: windowLocation.x,
        y: windowLocation.y,
        width: rect.width,
        height: rect.height,
      })
      const isVisible = rectangleContainsRectangle(visibleArea, windowRect)
      if (!isVisible) {
        actions.push(scrollToPosition(rect, 'to-center'))
      }
    }
    dispatch(actions)
  }, [
    dispatch,
    isOnAnotherRoute,
    remixLocationRoute,
    remixState,
    location,
    thread.id,
    commentScene,
    canvasScale,
    canvasOffset,
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
        <CommentRepliesCounter thread={thread} />
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
        const scrollArea = ref.current.offsetParent
        if (scrollArea != null) {
          const scrollAreaRect = scrollArea.getBoundingClientRect()
          const elementRect = ref.current.getBoundingClientRect()

          const fullyVisible =
            elementRect.top >= scrollAreaRect.top &&
            elementRect.bottom <= scrollAreaRect.bottom &&
            elementRect.left >= scrollAreaRect.left &&
            elementRect.right <= scrollAreaRect.right

          if (!fullyVisible) {
            ref.current.scrollIntoView()
          }
        }
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
