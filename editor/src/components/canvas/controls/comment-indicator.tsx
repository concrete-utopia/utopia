/** @jsxRuntime classic */
/** @jsx jsx */
import type { Interpolation } from '@emotion/react'
import { jsx } from '@emotion/react'
import type { ThreadData } from '@liveblocks/client'
import { Comment } from '@liveblocks/react-comments'
import { AnimatePresence, motion, useAnimate } from 'framer-motion'
import React from 'react'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useEditThreadMetadata, useStorage } from '../../../../liveblocks.config'
import {
  getCollaboratorById,
  useActiveThreads,
  useCanvasCommentThreadAndLocation,
  useCanvasLocationOfThread,
  useMyThreadReadStatus,
} from '../../../core/commenting/comment-hooks'
import type {
  CanvasPoint,
  CanvasRectangle,
  LocalPoint,
  MaybeInfinityCanvasRectangle,
  WindowPoint,
} from '../../../core/shared/math-utils'
import {
  canvasPoint,
  distance,
  getLocalPointInNewParentContext,
  isNotNullFiniteRectangle,
  localPoint,
  nullIfInfinity,
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
import { useMyUserId } from '../../../core/shared/multiplayer-hooks'
import { optionalMap } from '../../../core/shared/optional-utils'
import type { CommentWrapperProps } from '../../../utils/multiplayer-wrapper'
import { MultiplayerWrapper, baseMultiplayerAvatarStyle } from '../../../utils/multiplayer-wrapper'
import { when } from '../../../utils/react-conditionals'
import type { Theme } from '../../../uuiui'
import { UtopiaStyles, colorTheme } from '../../../uuiui'
import {
  setProp_UNSAFE,
  setRightMenuTab,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes, isCommentMode, isExistingComment } from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { RightMenuTab } from '../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MultiplayerAvatar } from '../../user-bar'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from '../dom-lookup'
import {
  RemixNavigationAtom,
  useRemixNavigationContext,
} from '../remix/utopia-remix-root-component'
import { CommentRepliesCounter } from './comment-replies-counter'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getIdOfScene, getSceneUnderPoint } from './comment-mode/comment-mode-hooks'
import * as EP from '../../../core/shared/element-path'
import { useRefAtom } from '../../editor/hook-utils'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'

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
          isRead={false}
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
  isRead: boolean
}

function getIndicatorStyle(
  position: WindowPoint,
  params: {
    isRead: boolean
    resolved: boolean
    isActive: boolean
    expanded: boolean
    dragging: boolean
  },
) {
  const { isRead, resolved, isActive, expanded, dragging } = params
  const canvasHeight = getCanvasHeight()

  const positionAdjust = 3 // px

  const base: Interpolation<Theme> = {
    pointerEvents: dragging ? 'none' : undefined,
    cursor: 'auto',
    padding: 2,
    position: 'fixed',
    bottom: canvasHeight - position.y - positionAdjust,
    left: position.x,
    background: isRead ? colorTheme.bg1.value : colorTheme.primary.value,
    borderRadius: '24px 24px 24px 0px',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
    border: '.4px solid #a3a3a340',
    opacity: resolved ? 0.6 : undefined,
    zIndex: expanded ? 1 : 'auto',
  }

  const whenActive: Interpolation<Theme> = {
    border: `1px solid ${colorTheme.primary.value}`,
  }

  const whenInactive: Interpolation<Theme> = {
    ':hover': {},
  }

  return {
    ...base,
    ...(isActive ? whenActive : whenInactive),
  }
}

export const CommentIndicatorUI = React.memo<CommentIndicatorUIProps>((props) => {
  const { position, bgColor, fgColor, avatarUrl, avatarInitials, resolved, isActive, isRead } =
    props

  return (
    <div
      css={getIndicatorStyle(position, {
        isRead: isRead ?? true,
        resolved: resolved,
        isActive: isActive,
        expanded: true,
        dragging: false,
      })}
    >
      <MultiplayerAvatar
        color={{ background: bgColor, foreground: fgColor }}
        picture={avatarUrl}
        name={avatarInitials}
      />
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

  const firstComment = React.useMemo(() => {
    return thread.comments[0]
  }, [thread])

  const user = React.useMemo(() => {
    return getCollaboratorById(collabs, firstComment.userId)
  }, [firstComment, collabs])

  const { location, scene: commentScene } = useCanvasLocationOfThread(thread)

  const readByMe = useMyThreadReadStatus(thread)
  const isRead = readByMe === 'read'

  const { hovered, onMouseOver, onMouseOut: onHoverMouseOut, cancelHover } = useHover()

  const [dragging, setDragging] = React.useState(false)
  const draggingCallback = React.useCallback((isDragging: boolean) => setDragging(isDragging), [])

  const { onMouseDown, didDrag, dragPosition } = useDragging(thread, location, draggingCallback)

  const remixLocationRoute = React.useMemo(() => {
    return thread.metadata.remixLocationRoute ?? null
  }, [thread])

  const remixState = useRemixNavigationContext(commentScene)

  const isOnAnotherRoute = React.useMemo(() => {
    return remixLocationRoute != null && remixLocationRoute !== remixState?.location.pathname
  }, [remixLocationRoute, remixState])

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

  const isActive = useEditorState(
    Substores.restOfEditor,
    (store) =>
      isCommentMode(store.editor.mode) &&
      store.editor.mode.comment != null &&
      isExistingComment(store.editor.mode.comment) &&
      store.editor.mode.comment.threadId === thread.id,
    'CommentIndicator isActive',
  )

  const preview = React.useMemo(() => {
    return !isActive && (hovered || dragging)
  }, [hovered, isActive, dragging])

  const position = React.useMemo(
    () => canvasPointToWindowPoint(dragPosition ?? location, canvasScale, canvasOffset),
    [location, canvasScale, canvasOffset, dragPosition],
  )

  const onMouseOut = React.useCallback(() => {
    if (dragging) {
      return
    }
    onHoverMouseOut()
  }, [dragging, onHoverMouseOut])

  const onClick = React.useCallback(() => {
    if (didDrag) {
      return
    }
    if (isOnAnotherRoute && remixLocationRoute != null && remixState != null) {
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

  // This is a hack: when the comment is unread, we show a dark background, so we need light foreground colors.
  // So we trick the Liveblocks Comment component and lie to it that the theme is dark mode.
  const dataThemeProp = isRead ? {} : { 'data-theme': 'dark' }

  return (
    <div
      onMouseOver={onMouseOver}
      onMouseOut={onMouseOut}
      onMouseDown={onMouseDown}
      onClick={onClick}
      data-testid='comment-indicator'
      css={getIndicatorStyle(position, {
        isRead: isRead,
        resolved: thread.metadata.resolved,
        isActive: isActive,
        expanded: preview,
        dragging: dragging,
      })}
    >
      <CommentIndicatorWrapper
        thread={thread}
        expanded={preview}
        user={user}
        comment={firstComment}
        showActions={false}
        style={{
          overflow: 'auto',
          background: 'transparent',
        }}
        {...dataThemeProp}
      />
    </div>
  )
})
CommentIndicator.displayName = 'CommentIndicator'

function canvasPositionOfThread(
  sceneGlobalFrame: CanvasRectangle,
  locationInScene: LocalPoint,
): CanvasPoint {
  return canvasPoint({
    x: sceneGlobalFrame.x + locationInScene.x,
    y: sceneGlobalFrame.y + locationInScene.y,
  })
}

function getThreadOriginalLocationOnCanvas(
  thread: ThreadData<ThreadMetadata>,
  startingSceneGlobalFrame: MaybeInfinityCanvasRectangle | null,
): CanvasPoint {
  const sceneId = thread.metadata.sceneId
  if (sceneId == null) {
    return canvasPoint({ x: thread.metadata.x, y: thread.metadata.y })
  }

  const globalFrame = nullIfInfinity(startingSceneGlobalFrame)
  if (globalFrame == null) {
    throw new Error('Found thread attached to scene with invalid global frame')
  }

  return canvasPositionOfThread(
    globalFrame,
    localPoint({ x: thread.metadata.x, y: thread.metadata.y }),
  )
}

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
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const dispatch = useDispatch()

  const scenesRef = useRefEditorState((store) =>
    MetadataUtils.getScenesMetadata(store.editor.jsxMetadata),
  )

  const remixSceneRoutesRef = useRefAtom(RemixNavigationAtom)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent) => {
      setDidDrag(false)
      draggingCallback(true)

      const mouseDownPoint = windowPoint({ x: event.clientX, y: event.clientY })
      const mouseDownCanvasPoint = windowToCanvasCoordinates(
        canvasScaleRef.current,
        canvasOffsetRef.current,
        windowPoint({ x: event.clientX, y: event.clientY }),
      ).canvasPositionRounded

      const maybeStartingSceneUnderPoint = getSceneUnderPoint(
        mouseDownCanvasPoint,
        scenesRef.current,
      )

      const originalThreadPosition = getThreadOriginalLocationOnCanvas(
        thread,
        maybeStartingSceneUnderPoint?.globalFrame ?? null,
      )

      let draggedPastThreshold = false
      function onMouseMove(moveEvent: MouseEvent) {
        moveEvent.stopPropagation()
        const mouseMovePoint = windowPoint({ x: moveEvent.clientX, y: moveEvent.clientY })

        draggedPastThreshold ||= distance(mouseDownPoint, mouseMovePoint) > COMMENT_DRAG_THRESHOLD

        if (draggedPastThreshold) {
          dispatch([switchEditorMode(EditorModes.commentMode(null, 'dragging'))])

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

        if (!draggedPastThreshold) {
          return
        }

        dispatch([switchEditorMode(EditorModes.commentMode(null, 'not-dragging'))])

        const dragVectorWindow = pointDifference(mouseDownPoint, mouseUpPoint)
        const dragVectorCanvas = canvasPoint({
          x: dragVectorWindow.x / canvasScaleRef.current,
          y: dragVectorWindow.y / canvasScaleRef.current,
        })
        setDragPosition(null)

        const newPositionOnCanvas = offsetPoint(originalThreadPosition, dragVectorCanvas)
        const maybeSceneUnderPoint = getSceneUnderPoint(newPositionOnCanvas, scenesRef.current)

        if (maybeSceneUnderPoint == null) {
          editThreadMetadata({
            threadId: thread.id,
            metadata: {
              x: newPositionOnCanvas.x,
              y: newPositionOnCanvas.y,
              sceneId: null,
              remixLocationRoute: null,
            },
          })
          return
        }

        const localPointInScene = isNotNullFiniteRectangle(maybeSceneUnderPoint.globalFrame)
          ? getLocalPointInNewParentContext(maybeSceneUnderPoint.globalFrame, newPositionOnCanvas)
          : newPositionOnCanvas

        const sceneId = getIdOfScene(maybeSceneUnderPoint)
        const sceneIdToUse = sceneId ?? EP.toUid(maybeSceneUnderPoint.elementPath)

        const remixRoute =
          remixSceneRoutesRef.current[EP.toString(maybeSceneUnderPoint?.elementPath)]

        if (sceneId == null) {
          dispatch([
            setProp_UNSAFE(
              maybeSceneUnderPoint.elementPath,
              PP.create('id'),
              jsExpressionValue(sceneIdToUse, emptyComments),
            ),
          ])
        }
        editThreadMetadata({
          threadId: thread.id,
          metadata: {
            x: localPointInScene.x,
            y: localPointInScene.y,
            sceneId: sceneIdToUse,
            remixLocationRoute: remixRoute != null ? remixRoute.location.pathname : undefined,
          },
        })
      }

      event.stopPropagation()
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
    },
    [
      draggingCallback,
      canvasScaleRef,
      canvasOffsetRef,
      scenesRef,
      thread,
      dispatch,
      originalLocation,
      remixSceneRoutesRef,
      editThreadMetadata,
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

type CommentIndicatorWrapper = {
  thread: ThreadData<ThreadMetadata>
  expanded: boolean
} & CommentWrapperProps

const CommentIndicatorWrapper = React.memo((props: CommentIndicatorWrapper) => {
  const { thread, expanded, user, ...commentProps } = props

  const [avatarRef, animateAvatar] = useAnimate()

  const animDuration = 0.1

  React.useEffect(() => {
    void animateAvatar(
      avatarRef.current,
      {
        top: expanded ? baseMultiplayerAvatarStyle.top : 0,
        left: expanded ? baseMultiplayerAvatarStyle.left : 0,
      },
      {
        duration: expanded ? animDuration : animDuration / 2,
      },
    )
  }, [expanded, avatarRef, animateAvatar])

  if (user == null) {
    return <Comment {...commentProps} />
  }

  return (
    <div
      data-testid='comment-indicator-wrapper'
      style={{
        position: 'relative',
        overflow: 'hidden',
        minWidth: baseMultiplayerAvatarStyle.width,
        minHeight: baseMultiplayerAvatarStyle.height,
      }}
    >
      <motion.div
        data-testid='comment-indicator-div'
        ref={avatarRef}
        style={{ ...baseMultiplayerAvatarStyle, left: 0, top: 0 }}
      >
        <MultiplayerAvatar
          name={multiplayerInitialsFromName(normalizeMultiplayerName(user.name))}
          color={multiplayerColorFromIndex(user.colorIndex)}
          picture={user.avatar}
          style={{ outline: 'none' }}
        />
      </motion.div>

      {/* AnimatePresence required to trigger the exit animation */}
      <AnimatePresence>
        {when(
          expanded,
          <motion.div
            initial={{
              width: 0,
              height: 0,
              opacity: 0,
            }}
            animate={{
              width: 250,
              height: 'auto',
              opacity: 1,
            }}
            exit={{
              width: 0,
              height: 0,
              opacity: 0,
            }}
            transition={{ duration: animDuration }}
          >
            <Comment {...commentProps} />
            <CommentRepliesCounter thread={thread} />
          </motion.div>,
        )}
      </AnimatePresence>
    </div>
  )
})

function getCanvasHeight(): number {
  const canvasDiv = document.getElementById('canvas-root')
  const canvasHeight = canvasDiv?.clientHeight ?? 0
  return canvasHeight
}
