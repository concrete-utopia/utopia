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
import {
  getFirstComment,
  openCommentThreadActions,
  sortThreadsByDescendingUpdateTimeInPlace,
} from '../../../core/shared/multiplayer'
import { getRemixLocationLabel } from '../../canvas/remix/remix-utils'
import type { RestOfEditorState } from '../../editor/store/store-hook-substore-types'
import type { EditorAction } from '../../editor/action-types'
import { canvasPointToWindowPoint } from '../../canvas/dom-lookup'
import { CommentRepliesCounter } from '../../canvas/controls/comment-replies-counter'

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

  const [filtersOpen, setOpen] = React.useState(false)
  const toggleOpen = React.useCallback(() => {
    setOpen((prevOpen) => !prevOpen)
  }, [setOpen])

  const { threads: threads } = useThreads()
  const { threads: resolvedThreads } = useResolvedThreads()
  const { threads: readThreads } = useReadThreads()

  sortThreadsByDescendingUpdateTimeInPlace(threads)
  sortThreadsByDescendingUpdateTimeInPlace(resolvedThreads)
  sortThreadsByDescendingUpdateTimeInPlace(readThreads)

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

  const [sortByDateNewestFirst, setSortByDateNewestFirst] = React.useState(true)
  const [sortByUnreadFirst, setSortedByUnreadFirst] = React.useState(false)

  const toggleSortByDateNewestFirst = React.useCallback(() => {
    setSortByDateNewestFirst((prevValue) => !prevValue)
  }, [])
  const toggleSortByUnreadFirst = React.useCallback(() => {
    setSortedByUnreadFirst((prevValue) => !prevValue)
  }, [])

  const sortedThreads = React.useMemo(() => {
    let filteredThreads = threads

    if (!showResolved) {
      filteredThreads = filteredThreads.filter((thread) => !resolvedThreads.includes(thread))
    }
    if (sortByDateNewestFirst) {
      filteredThreads = filteredThreads.slice().reverse()
    }
    if (sortByUnreadFirst) {
      const unreadThreads = filteredThreads.filter((thread) => !readThreads.includes(thread))
      const theReadThreads = filteredThreads.filter((thread) => readThreads.includes(thread))
      filteredThreads = [...unreadThreads, ...theReadThreads]
    }

    return filteredThreads
  }, [
    threads,
    resolvedThreads,
    readThreads,
    showResolved,
    sortByUnreadFirst,
    sortByDateNewestFirst,
  ])

  return (
    <FlexColumn>
      <FlexRow
        style={{
          flexGrow: 1,
          justifyContent: 'space-between',
          alignItems: 'center',
          fontWeight: 600,
          margin: 8,
        }}
      >
        <span>Comments</span>
        <Tooltip title='Sort/Filter' placement='bottom'>
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
                height: 14,
                width: 14,
                borderRadius: 14,
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
              <div
                style={{ width: 8, height: 1, background: colorTheme.fg1.value, borderRadius: 2 }}
              />
              <div
                style={{ width: 6, height: 1, background: colorTheme.fg1.value, borderRadius: 2 }}
              />
              <div
                style={{ width: 4, height: 1, background: colorTheme.fg1.value, borderRadius: 2 }}
              />
            </div>
          </div>
        </Tooltip>
      </FlexRow>
      {when(
        sortedThreads.length > 0,
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
              id='showNewestFirst'
              checked={sortByDateNewestFirst}
              onChange={toggleSortByDateNewestFirst}
            />
            <label htmlFor='showNewestFirst'>Newest First</label>
          </FlexRow>
          <FlexRow>
            <CheckboxInput
              style={{ marginRight: 8 }}
              id='showUnreadFirst'
              checked={sortByUnreadFirst}
              onChange={toggleSortByUnreadFirst}
            />
            <label htmlFor='showUnreadFirst'>Unread First</label>
          </FlexRow>
          {when(
            resolvedThreads.length > 0,
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
        </FlexColumn>,
      )}
      {when(
        sortedThreads.length === 0,
        <div style={{ padding: 8, color: colorTheme.fg6.value }}>
          Use the commenting tool to leave comments on the canvas. They will also show up here.
        </div>,
      )}
      {sortedThreads.map((thread) => (
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

  const comment = getFirstComment(thread)

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
