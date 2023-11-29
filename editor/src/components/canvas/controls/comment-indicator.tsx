/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { ThreadData } from '@liveblocks/client'
import { useAtom } from 'jotai'
import React from 'react'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useEditThreadMetadata, useStorage, useThreads } from '../../../../liveblocks.config'
import { useIsOnAnotherRemixRoute } from '../../../core/commenting/comment-hooks'
import type { CanvasPoint, CanvasVector, WindowPoint } from '../../../core/shared/math-utils'
import { canvasPoint, distance, offsetPoint, windowPoint } from '../../../core/shared/math-utils'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../../../core/shared/multiplayer'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { UtopiaStyles } from '../../../uuiui'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { EditorModes } from '../../editor/editor-modes'
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
  const { threads } = useThreads()

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

  const point = canvasPoint(thread.metadata)

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
    dispatch([switchEditorMode(EditorModes.commentMode(point, 'not-dragging'))])
  }, [dispatch, point, remixNavigationState, remixLocationRoute, isOnAnotherRoute])

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

  const { onMouseDown, dragPosition } = useDragging(thread)
  const position = React.useMemo(
    () => canvasPointToWindowPoint(dragPosition ?? point, canvasScale, canvasOffset),
    [point, canvasScale, canvasOffset, dragPosition],
  )

  return (
    <div
      css={{
        position: 'fixed',
        top: position.y,
        left: position.x,
        opacity: isOnAnotherRoute ? 0.25 : 1,
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

function useDragging(thread: ThreadData<ThreadMetadata>) {
  const editThreadMetadata = useEditThreadMetadata()
  const dispatch = useDispatch()
  const [dragPosition, setDragPosition] = React.useState<CanvasPoint | null>(null)

  const canvasScaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.realCanvasOffset)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent) => {
      const mouseDownPoint = windowPoint({ x: event.clientX, y: event.clientY })

      let draggedPastThreshold = false
      function onMouseMove(moveEvent: MouseEvent) {
        moveEvent.stopPropagation()
        const mouseMovePoint = windowPoint({ x: moveEvent.clientX, y: moveEvent.clientY })

        draggedPastThreshold ||= distance(mouseDownPoint, mouseMovePoint) > COMMENT_DRAG_THRESHOLD

        if (draggedPastThreshold) {
          const newCanvasPoint = mousePositionToIndicatorPosition(
            canvasScaleRef.current,
            canvasOffsetRef.current,
            mouseMovePoint,
          )
          setDragPosition(newCanvasPoint)
        }
      }

      function onMouseUp(upEvent: MouseEvent) {
        upEvent.stopPropagation()
        window.removeEventListener('mousemove', onMouseMove)
        window.removeEventListener('mouseup', onMouseUp)

        const mouseUpPoint = windowPoint({ x: upEvent.clientX, y: upEvent.clientY })

        if (draggedPastThreshold) {
          const newCanvasPosition = mousePositionToIndicatorPosition(
            canvasScaleRef.current,
            canvasOffsetRef.current,
            mouseUpPoint,
          )
          setDragPosition(null)

          editThreadMetadata({
            threadId: thread.id,
            metadata: {
              x: newCanvasPosition.x,
              y: newCanvasPosition.y,
            },
          })
        }
      }

      event.stopPropagation()
      dispatch([switchEditorMode(EditorModes.commentMode(null, 'dragging'))])
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
    },
    [dispatch, canvasOffsetRef, canvasScaleRef, editThreadMetadata, thread.id],
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
