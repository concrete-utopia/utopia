/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { EditorModes } from '../../editor/editor-modes'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import {
  useEditThreadMetadata,
  useSelf,
  useStorage,
  useThreads,
} from '../../../../liveblocks.config'
import { useDispatch } from '../../editor/store/dispatch-context'
import { switchEditorMode } from '../../editor/actions/action-creators'
import type { CanvasPoint, CanvasVector, WindowPoint } from '../../../core/shared/math-utils'
import { canvasPoint, offsetPoint, windowPoint } from '../../../core/shared/math-utils'
import { UtopiaTheme } from '../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../../../core/shared/multiplayer'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { AvatarPicture } from '../../user-bar'
import type { ThreadData } from '@liveblocks/client'
import { windowToCanvasCoordinates } from '../dom-lookup'

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
    <CanvasOffsetWrapper>
      <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
        <CommentIndicatorsInner />
      </MultiplayerWrapper>
    </CanvasOffsetWrapper>
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
  const me = useSelf()
  const dispatch = useDispatch()
  const collabs = useStorage((storage) => storage.collaborators)

  const position = canvasPoint(thread.metadata)

  const onClick = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.commentMode(position, 'not-dragging'))])
  }, [dispatch, position])

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

  const remixLocationRoute = thread.metadata.remixLocationRoute ?? null

  const isOnAnotherRoute =
    me.presence.remix?.locationRoute != null &&
    remixLocationRoute != null &&
    remixLocationRoute !== me.presence.remix.locationRoute

  const { onMouseDown, dragPosition } = useDragging(thread)

  return (
    <div
      css={{
        position: 'absolute',
        top: dragPosition?.y ?? position.y,
        left: dragPosition?.x ?? position.x,
        opacity: isOnAnotherRoute ? 0.25 : 1,
        width: IndicatorSize,
        '&:hover': {
          transform: 'scale(1.15)',
          transitionDuration: '0.1s',
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
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
          }}
        >
          <AvatarPicture url={avatar} initials={initials} />
        </div>
      </div>
    </div>
  )
})
CommentIndicator.displayName = 'CommentIndicator'

function useDragging(thread: ThreadData<ThreadMetadata>) {
  const editThreadMetadata = useEditThreadMetadata()
  const dispatch = useDispatch()
  const [dragPosition, setDragPosition] = React.useState<CanvasPoint | null>(null)

  const canvasScaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.realCanvasOffset)

  const onMouseMove = React.useCallback(
    (event: MouseEvent) => {
      event.stopPropagation()
      const newCanvasPoint = mousePositionToIndicatorPosition(
        canvasScaleRef.current,
        canvasOffsetRef.current,
        windowPoint({ x: event.clientX, y: event.clientY }),
      )
      setDragPosition(newCanvasPoint)
    },
    [canvasOffsetRef, canvasScaleRef],
  )

  const onMouseUp = React.useCallback(
    (event: MouseEvent) => {
      event.stopPropagation()
      const newCanvasPosition = mousePositionToIndicatorPosition(
        canvasScaleRef.current,
        canvasOffsetRef.current,
        windowPoint({ x: event.clientX, y: event.clientY }),
      )
      setDragPosition(null)

      editThreadMetadata({
        threadId: thread.id,
        metadata: {
          x: newCanvasPosition.x,
          y: newCanvasPosition.y,
        },
      })
      window.removeEventListener('mousemove', onMouseMove)
      window.removeEventListener('mouseup', onMouseUp)
    },
    [onMouseMove, canvasOffsetRef, canvasScaleRef, editThreadMetadata, thread.id],
  )

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent) => {
      event.stopPropagation()
      dispatch([switchEditorMode(EditorModes.commentMode(null, 'dragging'))])
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
    },
    [onMouseMove, onMouseUp, dispatch],
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
