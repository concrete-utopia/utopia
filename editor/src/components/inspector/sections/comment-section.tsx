/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import '../../../../resources/editor/css/liveblocks/react-comments/styles.css'
import '../../../../resources/editor/css/liveblocks/react-comments/dark/attributes.css'
import React from 'react'
import { Button, FlexColumn, FlexRow, Icn, PopupList, Tooltip, useColorTheme } from '../../../uuiui'
import { stopPropagation } from '../common/inspector-utils'
import { type ThreadMetadata, useThreads } from '../../../../liveblocks.config'
import type { ThreadData } from '@liveblocks/client'
import { useDispatch } from '../../editor/store/dispatch-context'
import { canvasRectangle, rectangleContainsRectangle } from '../../../core/shared/math-utils'
import {
  scrollToPosition,
  setCommentFilterMode,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes, isCommentMode, isExistingComment } from '../../editor/editor-modes'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { useRemixNavigationContext } from '../../canvas/remix/utopia-remix-root-component'
import {
  useResolveThread,
  useCanvasLocationOfThread,
  useMyThreadReadStatus,
  useReadThreads,
} from '../../../core/commenting/comment-hooks'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
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
import type { SelectOption } from '../controls/select-control'
import { assertNever } from '../../../core/shared/utils'
import { pluck } from '../../../core/shared/array-utils'
import { Comment } from '@liveblocks/react-comments'

export type CommentFilterMode = 'all' | 'all-including-resolved' | 'unread-only'

const filterOptions = [
  {
    label: 'All',
    value: 'all',
  },
  {
    label: 'All (and Resolved)',
    value: 'all-including-resolved',
  },
  {
    label: 'Unread Only',
    value: 'unread-only',
  },
]

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

  const { threads } = useThreads()
  const { threads: readThreads } = useReadThreads()

  sortThreadsByDescendingUpdateTimeInPlace(threads)

  const commentFilterMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.commentFilterMode,
    'ThreadPreviews commentFilterMode',
  )

  const filter = React.useMemo(
    () =>
      filterOptions.find((option) => option.value === commentFilterMode) ?? {
        label: 'All',
        value: 'all',
      },
    [commentFilterMode],
  )

  const sortedThreads = React.useMemo(() => {
    switch (commentFilterMode) {
      case 'all':
        return threads.filter((t) => !t.metadata.resolved)
      case 'all-including-resolved':
        return threads
      case 'unread-only':
        const readThreadIds = pluck(readThreads, 'id')
        return threads.filter((t) => !t.metadata.resolved && !readThreadIds.includes(t.id))
      default:
        assertNever(commentFilterMode)
    }
  }, [threads, readThreads, commentFilterMode])

  const handleSubmitValueFilter = React.useCallback(
    (option: SelectOption) => {
      dispatch([setCommentFilterMode(option.value)])
    },
    [dispatch],
  )

  return (
    <FlexColumn>
      <FlexRow
        style={{
          margin: 8,
          gap: 12,
          height: 22,
        }}
      >
        <span style={{ fontWeight: 600 }}>Comments</span>
        {when(
          threads.length > 0,
          <FlexRow
            style={{
              justifyContent: 'flex-end',
            }}
          >
            <PopupList
              value={filter}
              options={filterOptions}
              onSubmitValue={handleSubmitValueFilter}
              containerMode='noBorder'
            />
          </FlexRow>,
        )}
      </FlexRow>
      {when(
        sortedThreads.length === 0,
        <div
          style={{
            padding: 8,
            color: colorTheme.fg6.value,
            whiteSpace: 'normal',
            overflowWrap: 'break-word',
          }}
        >
          {commentFilterMode == 'unread-only'
            ? 'No Unread Comments.'
            : 'Use the commenting tool to leave comments on the canvas. They will also show up here.'}
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

  const editorRef = useRefEditorState((store) => ({
    canvasScale: store.editor.canvas.scale,
    canvasOffset: store.editor.canvas.roundedCanvasOffset,
  }))

  const onClick = React.useCallback(() => {
    if (isOnAnotherRoute) {
      if (remixState == null) {
        return
      }
      void remixState.navigate(remixLocationRoute)
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

      const windowLocation = canvasPointToWindowPoint(
        location,
        editorRef.current.canvasScale,
        editorRef.current.canvasOffset,
      )

      // adds a padding of 250px around `location`
      const windowRect = canvasRectangle({
        x: windowLocation.x - 250,
        y: windowLocation.y - 250,
        width: 500,
        height: 500,
      })
      const isVisible = rectangleContainsRectangle(visibleArea, windowRect)
      if (!isVisible) {
        const scrollToRect = canvasRectangle({
          x: location.x,
          y: location.y,
          width: 25,
          height: 25,
        })
        actions.push(scrollToPosition(scrollToRect, 'to-center'))
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
    editorRef,
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

  const isSelected = useIsSelectedAndScrollToThread(ref, thread.id)

  const comment = getFirstComment(thread)

  if (comment == null) {
    return null
  }

  const remixLocationRouteLabel = getRemixLocationLabel(remixLocationRoute)

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
        <Comment comment={comment} showActions={false} style={{ backgroundColor: 'transparent' }} />
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
      <Tooltip title={thread.metadata.resolved ? 'Unresolve' : 'Mark Resolved'} placement='top'>
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
