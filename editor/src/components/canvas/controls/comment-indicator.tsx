import type { ThreadData } from '@liveblocks/client'
import type { CommentProps } from '@liveblocks/react-comments'
import { Comment } from '@liveblocks/react-comments'
import { AnimatePresence, motion } from 'framer-motion'
import type { CSSProperties } from 'react'
import React from 'react'
import type { ThreadMetadata, UserMeta } from '../../../../liveblocks.config'
import { useEditThreadMetadata } from '../../../../liveblocks.config'
import {
  getCollaboratorById,
  getThreadLocationOnCanvas,
  useActiveThreads,
  useCanvasCommentThreadAndLocation,
  useCanvasLocationOfThread,
  useCollaborators,
  useMyThreadReadStatus,
} from '../../../core/commenting/comment-hooks'
import { utopiaThreadMetadataToLiveblocksPartial } from '../../../core/commenting/comment-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { CanvasPoint } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  distance,
  getLocalPointInNewParentContext,
  isNotNullFiniteRectangle,
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
import * as PP from '../../../core/shared/property-path'
import { MultiplayerWrapper, baseMultiplayerAvatarStyle } from '../../../utils/multiplayer-wrapper'
import { when } from '../../../utils/react-conditionals'
import { UtopiaStyles, colorTheme } from '../../../uuiui'
import {
  setProp_UNSAFE,
  setRightMenuTab,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes, isCommentMode, isExistingComment } from '../../editor/editor-modes'
import { useRefAtom } from '../../editor/hook-utils'
import { useDispatch } from '../../editor/store/dispatch-context'
import { RightMenuTab } from '../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MultiplayerAvatar } from '../../user-bar'
import { windowToCanvasCoordinates } from '../dom-lookup'
import {
  RemixNavigationAtom,
  useRemixNavigationContext,
} from '../remix/utopia-remix-root-component'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { getIdOfScene, getSceneUnderPoint } from './comment-mode/comment-mode-hooks'
import { CommentRepliesCounter } from './comment-replies-counter'

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
  position: CanvasPoint
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

  const { location } = useCanvasCommentThreadAndLocation(
    commentBeingComposed ?? { type: 'existing', threadId: 'dummy-thread-id' }, // this is as a placeholder for nulls
  )

  const collabs = useCollaborators()

  const myUserId = useMyUserId()

  const collaboratorInfo = React.useMemo(() => {
    const collaborator = optionalMap((id) => getCollaboratorById(collabs, id), myUserId)
    if (collaborator == null) {
      return {
        initials: 'AN',
        avatar: null,
      }
    }

    return {
      initials: multiplayerInitialsFromName(normalizeMultiplayerName(collaborator.name)),
      avatar: collaborator.avatar,
    }
  }, [collabs, myUserId])

  if (location == null) {
    return null
  }

  return {
    position: location,
    avatarUrl: collaboratorInfo.avatar ?? null,
    initials: collaboratorInfo.initials,
  }
}

const CommentIndicatorsInner = React.memo(() => {
  const threads = useActiveThreads()
  const temporaryIndicatorData = useCommentBeingComposed()

  return (
    <CanvasOffsetWrapper setScaleToo={true}>
      {threads.map((thread) => (
        <CommentIndicator key={thread.id} thread={thread} />
      ))}
      {temporaryIndicatorData != null ? (
        <CommentIndicatorUI
          position={temporaryIndicatorData.position}
          resolved={false}
          avatarUrl={temporaryIndicatorData.avatarUrl}
          avatarInitials={temporaryIndicatorData.initials}
          isActive={true}
          isRead={false}
        />
      ) : null}
    </CanvasOffsetWrapper>
  )
})
CommentIndicatorsInner.displayName = 'CommentIndicatorInner'

interface CommentIndicatorUIProps {
  position: CanvasPoint
  resolved: boolean
  avatarInitials: string
  avatarUrl?: string | null
  isActive: boolean
  isRead: boolean
}

function useIndicatorStyle(
  position: CanvasPoint,
  params: {
    isRead: boolean
    resolved: boolean
    isActive: boolean
    expanded: boolean
    dragging: boolean
  },
): CSSProperties {
  const { isRead, resolved, isActive, expanded, dragging } = params

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useIndicatorStyle canvasScale',
  )

  const style: CSSProperties = {
    pointerEvents: dragging ? 'none' : undefined,
    cursor: 'auto',
    padding: 2,
    position: 'absolute',
    left: position.x,
    top: position.y,
    // transform: 'translateY(-100%)',
    background: isRead ? colorTheme.bg1.value : colorTheme.primary.value,
    borderRadius: '24px 24px 24px 0px',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
    border: '.4px solid #a3a3a340',
    opacity: resolved ? 0.6 : undefined,
    zIndex: expanded ? 1 : 'auto',
    transform: `translateY(-100%) scale(${1 / canvasScale})`,
    transformOrigin: 'bottom left',
  }

  if (isActive) {
    style.border = `1px solid ${colorTheme.primary.value}`
  }

  return style
}

export const CommentIndicatorUI = React.memo<CommentIndicatorUIProps>((props) => {
  const { position, avatarUrl, avatarInitials, resolved, isActive, isRead } = props

  const style = useIndicatorStyle(position, {
    isRead: isRead ?? true,
    resolved: resolved,
    isActive: isActive,
    expanded: true,
    dragging: false,
  })

  return (
    <div style={style}>
      <MultiplayerAvatar
        color={multiplayerColorFromIndex(null)}
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

  const collabs = useCollaborators()

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

  const { onMouseDown: onMouseDownDrag, dragPosition } = useDragging(
    thread,
    location,
    draggingCallback,
  )

  const remixLocationRoute = React.useMemo(() => {
    return thread.metadata.remixLocationRoute ?? null
  }, [thread])

  const remixState = useRemixNavigationContext(commentScene)

  const isOnAnotherRoute = React.useMemo(() => {
    return remixLocationRoute != null && remixLocationRoute !== remixState?.location.pathname
  }, [remixLocationRoute, remixState])

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

  const onMouseOut = React.useCallback(() => {
    if (dragging) {
      return
    }
    onHoverMouseOut()
  }, [dragging, onHoverMouseOut])

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent) => {
      onMouseDownDrag(e, (dragged) => {
        if (dragged) {
          return
        }
        if (isOnAnotherRoute && remixLocationRoute != null && remixState != null) {
          void remixState.navigate(remixLocationRoute)
        }
        cancelHover()
        dispatch([
          ...openCommentThreadActions(thread.id, commentScene),
          setRightMenuTab(RightMenuTab.Comments),
        ])
      })
    },
    [
      dispatch,
      thread.id,
      remixState,
      remixLocationRoute,
      isOnAnotherRoute,
      commentScene,
      cancelHover,
      onMouseDownDrag,
    ],
  )

  const style = useIndicatorStyle(dragPosition ?? location, {
    isRead: isRead,
    resolved: thread.metadata.resolved,
    isActive: isActive,
    expanded: preview,
    dragging: dragging,
  })

  return (
    <div
      onMouseOver={onMouseOver}
      onMouseOut={onMouseOut}
      onMouseDown={onMouseDown}
      data-testid='comment-indicator'
      style={style}
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
        forceDarkMode={!isRead}
      />
    </div>
  )
})
CommentIndicator.displayName = 'CommentIndicator'

const COMMENT_DRAG_THRESHOLD = 5 // square px

function useDragging(
  thread: ThreadData<ThreadMetadata>,
  originalLocation: CanvasPoint,
  draggingCallback: (isDragging: boolean) => void,
) {
  const editThreadMetadata = useEditThreadMetadata()
  const [dragPosition, setDragPosition] = React.useState<CanvasPoint | null>(null)

  const canvasScaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const dispatch = useDispatch()

  const scenesRef = useRefEditorState((store) =>
    MetadataUtils.getScenesMetadata(store.editor.jsxMetadata),
  )

  const remixSceneRoutesRef = useRefAtom(RemixNavigationAtom)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent, onHandled: (dragged: boolean) => void) => {
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

      const originalThreadPosition = getThreadLocationOnCanvas(
        thread,
        maybeStartingSceneUnderPoint?.globalFrame ?? null,
      )

      let draggedPastThreshold = false
      function onMouseMove(moveEvent: MouseEvent) {
        moveEvent.stopPropagation()
        const mouseMovePoint = windowPoint({ x: moveEvent.clientX, y: moveEvent.clientY })

        draggedPastThreshold ||= distance(mouseDownPoint, mouseMovePoint) > COMMENT_DRAG_THRESHOLD

        if (draggedPastThreshold) {
          draggingCallback(true)
          dispatch([switchEditorMode(EditorModes.commentMode(null, 'dragging'))])

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

        draggingCallback(false)
        onHandled(draggedPastThreshold)

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
            metadata: utopiaThreadMetadataToLiveblocksPartial({
              type: 'canvas',
              position: newPositionOnCanvas,
            }),
          })
          return
        }

        const localPointInScene = isNotNullFiniteRectangle(maybeSceneUnderPoint.globalFrame)
          ? getLocalPointInNewParentContext(maybeSceneUnderPoint.globalFrame, newPositionOnCanvas)
          : null

        if (localPointInScene == null) {
          editThreadMetadata({
            threadId: thread.id,
            metadata: utopiaThreadMetadataToLiveblocksPartial({
              type: 'canvas',
              position: newPositionOnCanvas,
            }),
          })
          return
        }

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
          metadata: utopiaThreadMetadataToLiveblocksPartial({
            type: 'scene',
            position: newPositionOnCanvas,
            scenePosition: localPointInScene,
            sceneId: sceneIdToUse,
            remixLocationRoute: remixRoute != null ? remixRoute.location.pathname : undefined,
          }),
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

  return { onMouseDown, dragPosition }
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
  user: UserMeta | null
  expanded: boolean
  forceDarkMode: boolean
} & CommentProps

const CommentIndicatorWrapper = React.memo((props: CommentIndicatorWrapper) => {
  const { thread, expanded, user, forceDarkMode, ...commentProps } = props

  const animDuration = 0.1

  // This is a hack: when the comment is unread, we show a dark background, so we need light foreground colors.
  // So we trick the Liveblocks Comment component and lie to it that the theme is dark mode.
  const updatedCommentProps = forceDarkMode
    ? { ...commentProps, 'data-theme': 'dark' }
    : commentProps

  if (user == null) {
    return <Comment {...updatedCommentProps} />
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
        style={{ ...baseMultiplayerAvatarStyle, left: 0, top: 0 }}
        animate={{
          top: expanded ? baseMultiplayerAvatarStyle.top : 0,
          left: expanded ? baseMultiplayerAvatarStyle.left : 0,
          transition: {
            duration: expanded ? animDuration : animDuration / 2,
          },
        }}
      >
        <MultiplayerAvatar
          name={multiplayerInitialsFromName(normalizeMultiplayerName(user.name))}
          color={multiplayerColorFromIndex(null)}
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
            <Comment {...updatedCommentProps} />
            <CommentRepliesCounter thread={thread} forceDarkMode={forceDarkMode} />
          </motion.div>,
        )}
      </AnimatePresence>
    </div>
  )
})
