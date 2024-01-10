/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { Interpolation } from '@emotion/react'
import type { ThreadData } from '@liveblocks/client'
import React from 'react'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useEditThreadMetadata, useStorage } from '../../../../liveblocks.config'
import {
  useCanvasLocationOfThread,
  useActiveThreads,
  useCanvasCommentThreadAndLocation,
  useMyThreadReadStatus,
  getCollaboratorById,
} from '../../../core/commenting/comment-hooks'
import type { CanvasPoint, WindowPoint } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  distance,
  offsetPoint,
  pointDifference,
  windowPoint,
} from '../../../core/shared/math-utils'
import {
  getFirstComment,
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
  openCommentThreadActions,
} from '../../../core/shared/multiplayer'
import { CommentWrapper, MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import type { Theme } from '../../../uuiui'
import { UtopiaStyles, colorTheme } from '../../../uuiui'
import { isCommentMode, isExistingComment } from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { AvatarPicture } from '../../user-bar'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { useRemixNavigationContext } from '../remix/utopia-remix-root-component'
import { optionalMap } from '../../../core/shared/optional-utils'
import { setRightMenuTab } from '../../editor/actions/action-creators'
import { RightMenuTab } from '../../editor/store/editor-state'
import { when } from '../../../utils/react-conditionals'
import { CommentRepliesCounter } from './comment-replies-counter'
import { useMyUserId } from '../../../core/shared/multiplayer-hooks'

const IndicatorSize = 24
const MagnifyScale = 1.15

export const CommentIndicators = React.memo(() => {
  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'CommentIndicator projectId',
  )

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type,
    'CommentIndicators mode',
  )

  if (projectId == null || mode === 'live') {
    return null
  }

  return (
    <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
      <CommentIndicatorsInner />
    </MultiplayerWrapper>
  )
})
CommentIndicators.displayName = 'CommentIndicators'

interface TemporaryCommentIndicatorProps {
  position: WindowPoint
  bgColor: string
  fgColor: string
  avatarUrl: string | null
  initials: string
}

function useCommentBeingComposed(): TemporaryCommentIndicatorProps | null {
  const commentBeingComposed = useEditorState(
    Substores.restOfEditor,
    (store) => {
      if (store.editor.mode.type !== 'comment' || store.editor.mode.comment?.type !== 'new') {
        return null
      }
      return store.editor.mode.comment
    },
    'CommentIndicatorsInner commentBeingComposed',
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'MultiplayerPresence canvasScale',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'MultiplayerPresence canvasOffset',
  )

  const { location } = useCanvasCommentThreadAndLocation(
    commentBeingComposed ?? { type: 'existing', threadId: 'dummy-thread-id' }, // this is as a placeholder for nulls
  )
  const position = React.useMemo(
    () => (location == null ? null : canvasPointToWindowPoint(location, canvasScale, canvasOffset)),
    [location, canvasScale, canvasOffset],
  )

  const collabs = useStorage((storage) => storage.collaborators)

  const myUserId = useMyUserId()

  const collaboratorInfo = React.useMemo(() => {
    const collaborator = optionalMap((id) => collabs[id], myUserId)
    if (collaborator == null) {
      return {
        initials: 'AN',
        color: multiplayerColorFromIndex(null),
        avatar: null,
      }
    }

    return {
      initials: multiplayerInitialsFromName(normalizeMultiplayerName(collaborator.name)),
      color: multiplayerColorFromIndex(collaborator.colorIndex),
      avatar: collaborator.avatar,
    }
  }, [collabs, myUserId])

  if (position == null) {
    return null
  }

  return {
    position: position,
    bgColor: collaboratorInfo.color.background,
    fgColor: collaboratorInfo.color.foreground,
    avatarUrl: collaboratorInfo.avatar,
    initials: collaboratorInfo.initials,
  }
}

const CommentIndicatorsInner = React.memo(() => {
  const threads = useActiveThreads()
  const temporaryIndicatorData = useCommentBeingComposed()

  return (
    <React.Fragment>
      {threads.map((thread) => (
        <CommentIndicator key={thread.id} thread={thread} />
      ))}
      {temporaryIndicatorData != null ? (
        <CommentIndicatorUI
          position={temporaryIndicatorData.position}
          resolved={false}
          bgColor={temporaryIndicatorData.bgColor}
          fgColor={temporaryIndicatorData.fgColor}
          avatarUrl={temporaryIndicatorData.avatarUrl}
          avatarInitials={temporaryIndicatorData.initials}
          isActive={true}
        />
      ) : null}
    </React.Fragment>
  )
})
CommentIndicatorsInner.displayName = 'CommentIndicatorInner'

interface CommentIndicatorUIProps {
  position: WindowPoint
  resolved: boolean
  bgColor: string
  fgColor: string
  avatarInitials: string
  avatarUrl?: string | null
  isActive: boolean
  read?: boolean
}

export const CommentIndicatorUI = React.memo<CommentIndicatorUIProps>((props) => {
  const { position, bgColor, fgColor, avatarUrl, avatarInitials, resolved, isActive, read } = props

  const canvasScale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'CommentIndicatorUI scale',
  )

  function getIndicatorStyle() {
    const base: Interpolation<Theme> = {
      position: 'fixed',
      top: position.y + 3,
      left: position.x - 3,
      width: IndicatorSize,
      height: IndicatorSize,
      background: read ? colorTheme.bg1.value : colorTheme.primary.value,
      borderRadius: '24px 24px 24px 0px',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
      border: '.4px solid #a3a3a340',
      opacity: resolved ? 0.6 : 'undefined',
      zoom: 1 / canvasScale,
    }

    const transform: Interpolation<Theme> = {
      transform: `scale(${MagnifyScale})`,
      transformOrigin: 'bottom left',
      transitionDuration: 'transform 0.1s ease',
    }

    const whenActive: Interpolation<Theme> = {
      ...transform,
      border: `1px solid ${colorTheme.primary.value}`,
    }

    const whenInactive: Interpolation<Theme> = {
      ':hover': {
        ...transform,
      },
    }

    return {
      ...base,
      ...(isActive ? whenActive : whenInactive),
    }
  }

  return (
    <div css={getIndicatorStyle()}>
      <div
        style={{
          height: 18,
          width: 18,
          borderRadius: 18,
          background: bgColor,
          color: fgColor,
          fontSize: 9,
          fontWeight: 'bold',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <AvatarPicture url={avatarUrl} initials={avatarInitials} resolved={resolved} />
      </div>
    </div>
  )
})
CommentIndicatorUI.displayName = 'CommentIndicatorUI'

interface CommentIndicatorProps {
  thread: ThreadData<ThreadMetadata>
}

const CommentIndicator = React.memo(({ thread }: CommentIndicatorProps) => {
  const collabs = useStorage((storage) => storage.collaborators)

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'CommentIndicator canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'CommentIndicator canvasOffset',
  )

  const { location } = useCanvasLocationOfThread(thread)

  const readByMe = useMyThreadReadStatus(thread)

  const { initials, color, avatar } = (() => {
    const firstComment = thread.comments[0]
    if (firstComment == null) {
      return { initials: 'AN', color: multiplayerColorFromIndex(null), avatar: null }
    }
    const author = collabs[firstComment.userId]
    if (author == null) {
      return { initials: 'AN', color: multiplayerColorFromIndex(null), avatar: null }
    }
    return {
      initials: multiplayerInitialsFromName(normalizeMultiplayerName(author.name)),
      color: multiplayerColorFromIndex(author.colorIndex),
      avatar: author.avatar,
    }
  })()

  const position = React.useMemo(
    () => canvasPointToWindowPoint(location, canvasScale, canvasOffset),
    [location, canvasScale, canvasOffset],
  )

  const isActive = useEditorState(
    Substores.restOfEditor,
    (store) =>
      isCommentMode(store.editor.mode) &&
      store.editor.mode.comment != null &&
      isExistingComment(store.editor.mode.comment) &&
      store.editor.mode.comment.threadId === thread.id,
    'CommentIndicator isActive',
  )

  const { hovered, onMouseOver, onMouseOut, cancelHover } = useHover()

  const [dragging, setDragging] = React.useState(false)

  const draggingCallback = React.useCallback((isDragging: boolean) => setDragging(isDragging), [])

  return (
    <div
      onMouseOver={onMouseOver}
      onMouseOut={onMouseOut}
      data-testid='comment-indicator'
      style={{ cursor: 'auto' }}
    >
      {when(
        (isActive || !hovered) && !dragging,
        <CommentIndicatorUI
          position={position}
          resolved={thread.metadata.resolved}
          bgColor={color.background}
          fgColor={color.foreground}
          avatarUrl={avatar}
          avatarInitials={initials}
          isActive={isActive}
          read={readByMe === 'read'}
        />,
      )}

      {when(
        !isActive,
        <HoveredCommentIndicator
          thread={thread}
          hidden={!hovered}
          cancelHover={cancelHover}
          draggingCallback={draggingCallback}
        />,
      )}
    </div>
  )
})
CommentIndicator.displayName = 'CommentIndicator'

interface HoveredCommentIndicatorProps {
  thread: ThreadData<ThreadMetadata>
  hidden: boolean
  cancelHover: () => void
  draggingCallback: (isDragging: boolean) => void
}

const HoveredCommentIndicator = React.memo((props: HoveredCommentIndicatorProps) => {
  const { thread, hidden, cancelHover, draggingCallback } = props

  const dispatch = useDispatch()

  const { location, scene: commentScene } = useCanvasLocationOfThread(thread)

  const { onMouseDown, didDrag, dragPosition } = useDragging(thread, location, draggingCallback)

  const remixLocationRoute = thread.metadata.remixLocationRoute ?? null

  const remixState = useRemixNavigationContext(commentScene)

  const isOnAnotherRoute =
    remixLocationRoute != null && remixLocationRoute !== remixState?.location.pathname

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'HoveredCommentIndicator canvasScale',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'HoveredCommentIndicator canvasOffset',
  )

  const position = React.useMemo(
    () => canvasPointToWindowPoint(dragPosition ?? location, canvasScale, canvasOffset),
    [location, canvasScale, canvasOffset, dragPosition],
  )

  const onClick = React.useCallback(() => {
    if (didDrag) {
      return
    }
    if (isOnAnotherRoute) {
      if (remixState == null) {
        return
      }
      remixState.navigate(remixLocationRoute)
    }
    cancelHover()
    dispatch([
      ...openCommentThreadActions(thread.id, commentScene),
      setRightMenuTab(RightMenuTab.Comments),
    ])
  }, [
    dispatch,
    thread.id,
    remixState,
    remixLocationRoute,
    isOnAnotherRoute,
    commentScene,
    didDrag,
    cancelHover,
  ])

  const canvasDiv = document.getElementById('canvas-root')

  const canvasHeight = canvasDiv?.clientHeight ?? 0

  const collabs = useStorage((storage) => storage.collaborators)
  if (hidden && dragPosition == null) {
    return null
  }

  const comment = getFirstComment(thread)
  if (comment == null) {
    return null
  }
  const user = getCollaboratorById(collabs, comment.userId)
  if (user == null) {
    return null
  }

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        borderRadius: '18px 18px 18px 0px',
        width: 250,
        boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
        background: colorTheme.bg1.value,
        border: '.4px solid #a3a3a340',
        zIndex: 1,
        position: 'fixed',
        bottom: canvasHeight - IndicatorSize - position.y,
        // temporarily moving the hovered comment indicator to align with the not hovered version
        left: position.x - 3,
        overflow: 'hidden',
        zoom: 1 / canvasScale,
      }}
      onMouseDown={onMouseDown}
      onClick={onClick}
    >
      <CommentWrapper
        style={{
          overflow: 'auto',
          background: 'transparent',
        }}
        user={user}
        comment={comment}
        showActions={false}
      />
      <CommentRepliesCounter thread={thread} />
    </div>
  )
})
HoveredCommentIndicator.displayName = 'HoveredCommentIndicator'

const COMMENT_DRAG_THRESHOLD = 5 // square px

function useDragging(
  thread: ThreadData<ThreadMetadata>,
  originalLocation: CanvasPoint,
  draggingCallback: (isDragging: boolean) => void,
) {
  const editThreadMetadata = useEditThreadMetadata()
  const [dragPosition, setDragPosition] = React.useState<CanvasPoint | null>(null)
  const [didDrag, setDidDrag] = React.useState(false)

  const canvasScaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent) => {
      setDidDrag(false)
      draggingCallback(true)

      const mouseDownPoint = windowPoint({ x: event.clientX, y: event.clientY })

      let draggedPastThreshold = false
      function onMouseMove(moveEvent: MouseEvent) {
        moveEvent.stopPropagation()
        const mouseMovePoint = windowPoint({ x: moveEvent.clientX, y: moveEvent.clientY })

        draggedPastThreshold ||= distance(mouseDownPoint, mouseMovePoint) > COMMENT_DRAG_THRESHOLD

        if (draggedPastThreshold) {
          setDidDrag(true)
          const dragVectorWindow = pointDifference(mouseDownPoint, mouseMovePoint)
          const dragVectorCanvas = canvasPoint({
            x: dragVectorWindow.x / canvasScaleRef.current,
            y: dragVectorWindow.y / canvasScaleRef.current,
          })
          setDragPosition(offsetPoint(originalLocation, dragVectorCanvas))
        }
      }

      function onMouseUp(upEvent: MouseEvent) {
        upEvent.stopPropagation()
        window.removeEventListener('mousemove', onMouseMove)
        window.removeEventListener('mouseup', onMouseUp)
        draggingCallback(false)

        const mouseUpPoint = windowPoint({ x: upEvent.clientX, y: upEvent.clientY })

        if (draggedPastThreshold) {
          const dragVectorWindow = pointDifference(mouseDownPoint, mouseUpPoint)
          const dragVectorCanvas = canvasPoint({
            x: dragVectorWindow.x / canvasScaleRef.current,
            y: dragVectorWindow.y / canvasScaleRef.current,
          })
          setDragPosition(null)

          editThreadMetadata({
            threadId: thread.id,
            metadata: offsetPoint(canvasPoint(thread.metadata), dragVectorCanvas),
          })
        }
      }

      event.stopPropagation()
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
    },
    [
      canvasScaleRef,
      editThreadMetadata,
      thread.id,
      originalLocation,
      thread.metadata,
      draggingCallback,
    ],
  )

  return { onMouseDown, dragPosition, didDrag }
}

function useHover() {
  const [hovered, setHovered] = React.useState(false)

  const onMouseOver = React.useCallback(() => {
    setHovered(true)
  }, [])

  const onMouseOut = React.useCallback(() => {
    setHovered(false)
  }, [])

  const cancelHover = React.useCallback(() => {
    setHovered(false)
  }, [])

  return { hovered, onMouseOver, onMouseOut, cancelHover }
}
