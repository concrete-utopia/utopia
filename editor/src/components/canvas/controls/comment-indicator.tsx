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
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
  openCommentThreadActions,
} from '../../../core/shared/multiplayer'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { setRightMenuTab, switchEditorMode } from '../../editor/actions/action-creators'
import type { Theme } from '../../../uuiui'
import { UtopiaStyles, colorTheme } from '../../../uuiui'
import { EditorModes, isCommentMode, isExistingComment } from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { AvatarPicture } from '../../user-bar'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { RightMenuTab } from '../../editor/store/editor-state'
import { useRemixNavigationContext } from '../remix/utopia-remix-root-component'
import { assertNever } from '../../../core/shared/utils'
import { optionalMap } from '../../../core/shared/optional-utils'

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

  const userId = useEditorState(
    Substores.userState,
    (store) => {
      if (store.userState.loginState.type !== 'LOGGED_IN') {
        return null
      }

      return store.userState.loginState.user.userId
    },
    'CommentThread userId',
  )

  const collaboratorInfo = React.useMemo(() => {
    const collaborator = optionalMap((id) => collabs[id], userId)
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
  }, [collabs, userId])

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
          opacity={1}
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
  opacity: number
  resolved: boolean
  bgColor: string
  fgColor: string
  avatarInitials: string
  avatarUrl?: string | null
  onClick?: (e: React.MouseEvent) => void
  onMouseDown?: (e: React.MouseEvent) => void
  isActive: boolean
  read?: boolean
}

export const CommentIndicatorUI = React.memo<CommentIndicatorUIProps>((props) => {
  const {
    position,
    onClick,
    onMouseDown,
    bgColor,
    fgColor,
    avatarUrl,
    avatarInitials,
    opacity,
    resolved,
    isActive,
    read,
  } = props

  function getIndicatorStyle() {
    const base: Interpolation<Theme> = {
      position: 'fixed',
      top: position.y + 3,
      left: position.x - 3,
      opacity: opacity,
      filter: resolved ? 'grayscale(1)' : undefined,
      width: IndicatorSize,
      height: IndicatorSize,
      background: read ? colorTheme.bg1.value : colorTheme.primary.value,
      borderRadius: '24px 24px 24px 0px',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
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
    <div onClick={onClick} onMouseDown={onMouseDown} css={getIndicatorStyle()}>
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
        <AvatarPicture url={avatarUrl} initials={avatarInitials} />
      </div>
    </div>
  )
})
CommentIndicatorUI.displayName = 'CommentIndicatorUI'

interface CommentIndicatorProps {
  thread: ThreadData<ThreadMetadata>
}

const CommentIndicator = React.memo(({ thread }: CommentIndicatorProps) => {
  const dispatch = useDispatch()
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

  const { location, scene: commentScene } = useCanvasLocationOfThread(thread)

  const remixLocationRoute = thread.metadata.remixLocationRoute ?? null

  const remixState = useRemixNavigationContext(commentScene)

  const isOnAnotherRoute =
    remixLocationRoute != null && remixLocationRoute !== remixState?.location.pathname

  const readByMe = useMyThreadReadStatus(thread)

  const onClick = React.useCallback(() => {
    if (isOnAnotherRoute) {
      if (remixState == null) {
        return
      }
      remixState.navigate(remixLocationRoute)
    }
    dispatch(openCommentThreadActions(thread.id, commentScene))
  }, [dispatch, thread.id, remixState, remixLocationRoute, isOnAnotherRoute, commentScene])

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

  const { onMouseDown, dragPosition } = useDragging(thread, location)
  const position = React.useMemo(
    () => canvasPointToWindowPoint(dragPosition ?? location, canvasScale, canvasOffset),
    [location, canvasScale, canvasOffset, dragPosition],
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

  return (
    <CommentIndicatorUI
      position={position}
      opacity={isOnAnotherRoute || thread.metadata.resolved ? 0 : 1}
      resolved={thread.metadata.resolved}
      onClick={onClick}
      onMouseDown={onMouseDown}
      bgColor={color.background}
      fgColor={color.foreground}
      avatarUrl={avatar}
      avatarInitials={initials}
      isActive={isActive}
      read={readByMe === 'read'}
    />
  )
})
CommentIndicator.displayName = 'CommentIndicator'

const COMMENT_DRAG_THRESHOLD = 5 // square px

function useDragging(thread: ThreadData<ThreadMetadata>, originalLocation: CanvasPoint) {
  const editThreadMetadata = useEditThreadMetadata()
  const dispatch = useDispatch()
  const [dragPosition, setDragPosition] = React.useState<CanvasPoint | null>(null)

  const canvasScaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent) => {
      const mouseDownPoint = windowPoint({ x: event.clientX, y: event.clientY })

      let draggedPastThreshold = false
      function onMouseMove(moveEvent: MouseEvent) {
        moveEvent.stopPropagation()
        const mouseMovePoint = windowPoint({ x: moveEvent.clientX, y: moveEvent.clientY })

        draggedPastThreshold ||= distance(mouseDownPoint, mouseMovePoint) > COMMENT_DRAG_THRESHOLD

        if (draggedPastThreshold) {
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
      dispatch([
        switchEditorMode(EditorModes.commentMode(null, 'dragging')),
        setRightMenuTab(RightMenuTab.Comments),
      ])
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
    },
    [dispatch, canvasScaleRef, editThreadMetadata, thread.id, originalLocation, thread.metadata],
  )

  return { onMouseDown, dragPosition }
}
