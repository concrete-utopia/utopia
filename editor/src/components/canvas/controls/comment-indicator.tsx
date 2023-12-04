/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { ThreadData } from '@liveblocks/client'
import { useAtom } from 'jotai'
import React from 'react'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useEditThreadMetadata, useStorage } from '../../../../liveblocks.config'
import {
  useCanvasLocationOfThread,
  useIsOnAnotherRemixRoute,
  useActiveThreads,
} from '../../../core/commenting/comment-hooks'
import type { CanvasPoint, CanvasVector, WindowPoint } from '../../../core/shared/math-utils'
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
} from '../../../core/shared/multiplayer'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { UtopiaStyles } from '../../../uuiui'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { EditorModes, existingComment } from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { AvatarPicture } from '../../user-bar'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from '../dom-lookup'
import { RemixNavigationAtom } from '../remix/utopia-remix-root-component'

const IndicatorSize = 20

export const CommentIndicators = React.memo(() => {
  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'CommentIndicator projectId',
  )

  if (projectId == null) {
    return null
  }

  return (
    <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
      <CommentIndicatorsInner />
    </MultiplayerWrapper>
  )
})
CommentIndicators.displayName = 'CommentIndicators'

const CommentIndicatorsInner = React.memo(() => {
  const threads = useActiveThreads()

  return (
    <React.Fragment>
      {threads.map((thread) => (
        <CommentIndicator key={thread.id} thread={thread} />
      ))}
    </React.Fragment>
  )
})
CommentIndicatorsInner.displayName = 'CommentIndicatorInner'

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

  const [remixNavigationState] = useAtom(RemixNavigationAtom)

  const canvasLocation = useCanvasLocationOfThread(thread)

  const remixLocationRoute = thread.metadata.remixLocationRoute ?? null

  const isOnAnotherRoute = useIsOnAnotherRemixRoute(remixLocationRoute)

  const onClick = React.useCallback(() => {
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
    ])
  }, [dispatch, thread.id, remixNavigationState, remixLocationRoute, isOnAnotherRoute])

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

  const { onMouseDown, dragPosition } = useDragging(thread, canvasLocation)
  const position = React.useMemo(
    () => canvasPointToWindowPoint(dragPosition ?? canvasLocation, canvasScale, canvasOffset),
    [canvasLocation, canvasScale, canvasOffset, dragPosition],
  )

  return (
    <div
      css={{
        position: 'fixed',
        top: position.y,
        left: position.x,
        opacity: isOnAnotherRoute || thread.metadata.resolved ? 0.25 : 1,
        filter: thread.metadata.resolved ? 'grayscale(1)' : undefined,
        width: IndicatorSize,
        '&:hover': {
          transform: 'scale(1.15)',
          transitionDuration: 'transform 0.1s',
        },
      }}
      onClick={onClick}
      onMouseDown={onMouseDown}
    >
      <div
        css={{
          height: 24,
          width: 24,
          background: 'black',
          border: '1px solid ',
          borderRadius: '24px 24px 24px 0px',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          style={{
            height: 20,
            width: 20,
            borderRadius: 10,
            background: color.background,
            color: color.foreground,
            fontSize: 9,
            fontWeight: 'bold',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
          }}
        >
          <AvatarPicture url={avatar} initials={initials} />
        </div>
      </div>
    </div>
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
      dispatch([switchEditorMode(EditorModes.commentMode(null, 'dragging'))])
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
    },
    [dispatch, canvasScaleRef, editThreadMetadata, thread.id, originalLocation, thread.metadata],
  )

  return { onMouseDown, dragPosition }
}

function mousePositionToIndicatorPosition(
  canvasScale: number,
  canvasOffset: CanvasVector,
  windowPosition: WindowPoint,
): CanvasPoint {
  return offsetPoint(
    windowToCanvasCoordinates(
      canvasScale,
      canvasOffset,
      windowPoint({ x: windowPosition.x, y: windowPosition.y }),
    ).canvasPositionRounded,
    canvasPoint({ x: -IndicatorSize / 2, y: -IndicatorSize / 2 }),
  )
}
