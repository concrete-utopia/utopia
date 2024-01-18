import type { User } from '@liveblocks/client'
import { motion } from 'framer-motion'
import { useAtom, useSetAtom } from 'jotai'
import React from 'react'
import type { Presence, PresenceActiveFrame, UserMeta } from '../../../liveblocks.config'
import {
  useOthers,
  useOthersListener,
  useRoom,
  useSelf,
  useStorage,
  useUpdateMyPresence,
} from '../../../liveblocks.config'
import {
  getCollaborator,
  useAddMyselfToCollaborators,
  useCanComment,
} from '../../core/commenting/comment-hooks'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import type { CanvasPoint } from '../../core/shared/math-utils'
import {
  canvasPoint,
  pointsEqual,
  scaleRect,
  windowPoint,
  zeroRectIfNullOrInfinity,
} from '../../core/shared/math-utils'
import {
  isPlayerOnAnotherRemixRoute,
  multiplayerColorFromIndex,
  normalizeOthersList,
  useIsOnSameRemixRoute,
} from '../../core/shared/multiplayer'
import { assertNever } from '../../core/shared/utils'
import { UtopiaStyles, useColorTheme } from '../../uuiui'
import { notice } from '../common/notice'
import type { EditorAction } from '../editor/action-types'
import { isLoggedIn } from '../editor/action-types'
import { showToast, switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes, isCommentMode, isFollowMode } from '../editor/editor-modes'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../editor/store/store-hook'
import CanvasActions from './canvas-actions'
import { activeFrameActionToString } from './commands/set-active-frames-command'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import { ActiveRemixSceneAtom, RemixNavigationAtom } from './remix/utopia-remix-root-component'
import { useMyUserId, useRemixPresence } from '../../core/shared/multiplayer-hooks'
import { CanvasOffsetWrapper } from './controls/canvas-offset-wrapper'
import { when } from '../../utils/react-conditionals'
import { CommentIndicators } from './controls/comment-indicator'
import { CommentPopup } from './controls/comment-popup'
import { getSceneUnderPoint } from './controls/comment-mode/comment-mode-hooks'
import { getRemixSceneDataLabel } from './remix/remix-utils'

export const OtherUserPointer = (props: any) => {
  return (
    <svg width='12' height='17' viewBox='0 0 12 17' fill='none' xmlns='http://www.w3.org/2000/svg'>
      <path
        id='Default'
        d='M0.0397339 0.0756531V16.5757L4.16473 11.0757H11.0397L0.0397339 0.0756531Z'
        fill={props.color}
        stroke='white'
      />
    </svg>
  )
}

export const MultiplayerPresence = React.memo(() => {
  const dispatch = useDispatch()

  const room = useRoom()
  const updateMyPresence = useUpdateMyPresence()

  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'MultiplayerPresence loginState',
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
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'MultiplayerPresence mode',
  )

  useAddMyselfToCollaborators()

  const remixPresence = useRemixPresence()

  React.useEffect(() => {
    if (!isLoggedIn(loginState)) {
      return
    }
    updateMyPresence({
      canvasScale,
      canvasOffset,
      following: isFollowMode(mode) ? mode.playerId : null,
      remix: remixPresence,
    })
  }, [canvasScale, canvasOffset, updateMyPresence, loginState, mode, remixPresence])

  React.useEffect(() => {
    // when the mouse moves over the canvas, update the presence cursor
    function onMouseMove(e: MouseEvent) {
      updateMyPresence({
        cursor: windowPoint({ x: e.clientX, y: e.clientY }),
      })
    }
    window.addEventListener('mousemove', onMouseMove)
    return function () {
      window.removeEventListener('mousemove', onMouseMove)
    }
  }, [updateMyPresence])

  const [roomId, setRoomId] = React.useState<string>(room.id)

  React.useEffect(() => {
    // when the room changes, reset
    // (unfortunately there's not a subscription event for room id changes :()
    if (roomId !== room.id) {
      setRoomId(room.id)
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
    }
  }, [room.id, roomId, dispatch])

  const canComment = useCanComment()

  if (!isLoggedIn(loginState)) {
    return null
  }

  return (
    <>
      <FollowingOverlay />
      <MultiplayerShadows />
      {when(canComment, <CommentIndicators />)}
      <MultiplayerCursors />
      {when(canComment && isCommentMode(mode) && mode.comment != null, <CommentPopup />)}
    </>
  )
})
MultiplayerPresence.displayName = 'MultiplayerPresence'

const MultiplayerCursors = React.memo(() => {
  const me = useSelf()
  const collabs = useStorage((store) => store.collaborators)
  const others = useOthers((list) => {
    const presences = normalizeOthersList(me.id, list)
    return presences.map((p) => ({
      presenceInfo: p,
      userInfo: getCollaborator(collabs, p),
    }))
  })

  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        pointerEvents: 'none',
      }}
    >
      {others.map((other) => {
        if (
          other.presenceInfo.presence.cursor == null ||
          other.presenceInfo.presence.canvasOffset == null ||
          other.presenceInfo.presence.canvasScale == null
        ) {
          return null
        }
        const position = windowToCanvasCoordinates(
          other.presenceInfo.presence.canvasScale,
          other.presenceInfo.presence.canvasOffset,
          other.presenceInfo.presence.cursor,
        ).canvasPositionRounded
        return (
          <MultiplayerCursor
            key={`cursor-${other.presenceInfo.id}`}
            name={other.userInfo.name}
            colorIndex={other.userInfo.colorIndex}
            position={position}
            userId={other.userInfo.id}
          />
        )
      })}
    </div>
  )
})
MultiplayerCursors.displayName = 'MultiplayerCursors'

function useGhostPointerState(position: CanvasPoint, userId: string) {
  const [shouldShowGhostPointer, setShouldShowGhostPointer] = React.useState<boolean>(false)

  const scenesRef = useRefEditorState((store) =>
    MetadataUtils.getScenesMetadata(store.editor.jsxMetadata),
  )

  const isOnSameRemixRoute = useIsOnSameRemixRoute()

  const timeoutHandle = React.useRef<ReturnType<typeof setTimeout> | null>(null)
  React.useEffect(() => {
    timeoutHandle.current = setTimeout(() => {
      timeoutHandle.current = null
      const instance =
        // making a new canvasPoint here so that the memo array contains only primitive types
        getSceneUnderPoint(canvasPoint({ x: position.x, y: position.y }), scenesRef.current)
      const dataLabel = getRemixSceneDataLabel(instance)

      if (instance == null || dataLabel == null) {
        setShouldShowGhostPointer(false)
      } else {
        setShouldShowGhostPointer(
          !isOnSameRemixRoute({ otherUserId: userId, remixSceneDataLabel: dataLabel }),
        )
      }
    }, 1000)

    return () => {
      if (timeoutHandle.current != null) {
        clearTimeout(timeoutHandle.current)
      }
    }
  }, [isOnSameRemixRoute, position.x, position.y, scenesRef, userId])

  return shouldShowGhostPointer
}

const MultiplayerCursor = React.memo(
  ({
    name,
    colorIndex,
    position,
    userId,
  }: {
    name: string | null
    colorIndex: number | null
    position: CanvasPoint
    userId: string
  }) => {
    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'MultiplayerCursor canvasScale',
    )
    const color = multiplayerColorFromIndex(colorIndex)

    const shouldShowGhostPointer = useGhostPointerState(position, userId)

    return (
      <CanvasOffsetWrapper setScaleToo={true}>
        <motion.div
          initial={position}
          animate={position}
          transition={{
            type: 'spring',
            damping: 30,
            mass: 0.8,
            stiffness: 350,
          }}
          style={{
            scale: canvasScale <= 1 ? 1 / canvasScale : 1,
            opacity: shouldShowGhostPointer ? 0.3 : 1,
          }}
        >
          <div
            style={{
              filter: 'drop-shadow(1px 2px 3px rgb(0 0 0 / 0.3))',
              transform: 'translate(0px, 0px)',
              zoom: canvasScale > 1 ? 1 / canvasScale : 1,
            }}
          >
            <OtherUserPointer color={color.background} />
          </div>
          <div
            style={{
              color: color.foreground,
              backgroundColor: color.background,
              padding: '0 4px',
              borderRadius: 2,
              boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
              fontWeight: 'bold',
              fontSize: 9,
              position: 'absolute',
              left: 6,
              top: 15,
              zoom: canvasScale > 1 ? 1 / canvasScale : 1,
            }}
          >
            {name}
          </div>
        </motion.div>
      </CanvasOffsetWrapper>
    )
  },
)
MultiplayerCursor.displayName = 'MultiplayerCursor'

const remixRouteChangedToastId = 'follow-changed-scene'

const FollowingOverlay = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const room = useRoom()

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'FollowingOverlay mode',
  )
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'FollowingOverlay canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FollowingOverlay canvasOffset',
  )

  const isFollowTarget = React.useCallback(
    (other: User<Presence, UserMeta>): boolean => {
      return (
        isFollowMode(mode) && other.id === mode.playerId && other.connectionId === mode.connectionId
      )
    },
    [mode],
  )

  const resetFollowed = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
  }, [dispatch])

  const followed = React.useMemo(() => {
    return room.getOthers().find(isFollowTarget) ?? null
  }, [room, isFollowTarget])

  const followedUser = useStorage((store) =>
    followed != null ? store.collaborators[followed.id] : null,
  )

  const remixPresence = useRemixPresence()

  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)
  const [remixNavigationState] = useAtom(RemixNavigationAtom)

  const updateCanvasFromOtherPresence = React.useCallback(
    (presence: Presence) => {
      let actions: EditorAction[] = []
      if (presence.canvasScale != null && presence.canvasScale !== canvasScale) {
        actions.push(CanvasActions.zoom(presence.canvasScale, null))
      }
      if (presence.canvasOffset != null && !pointsEqual(presence.canvasOffset, canvasOffset)) {
        actions.push(CanvasActions.positionCanvas(presence.canvasOffset))
      }
      if (presence.remix != null && isPlayerOnAnotherRemixRoute(remixPresence, presence.remix)) {
        const sceneState = remixNavigationState[presence.remix.scene]
        if (sceneState != null && presence.remix.locationRoute != null) {
          setActiveRemixScene(EP.fromString(presence.remix.scene))
          sceneState.navigate(presence.remix.locationRoute)
          actions.push(
            showToast(
              notice(
                `The route has been changed to ${presence.remix.locationRoute}`,
                'PRIMARY',
                false,
                remixRouteChangedToastId,
              ),
            ),
          )
        }
      }
      if (actions.length > 0) {
        dispatch(actions)
      }
    },
    [dispatch, canvasScale, canvasOffset, setActiveRemixScene, remixPresence, remixNavigationState],
  )

  useOthersListener((event) => {
    if (isFollowMode(mode)) {
      switch (event.type) {
        case 'enter':
        case 'update':
          if (isFollowTarget(event.user)) {
            updateCanvasFromOtherPresence(event.user.presence)
          }
          break
        case 'leave':
          if (isFollowTarget(event.user)) {
            resetFollowed()
          }
          break
        case 'reset':
          resetFollowed()
          break
        default:
          assertNever(event)
      }
    }
  })

  if (followed == null || followedUser == null) {
    return null
  }
  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        bottom: 0,
        right: 0,
        background: 'transparent',
        display: 'flex',
        alignItems: 'flex-end',
        justifyContent: 'center',
        paddingBottom: 14,
        cursor: 'default',
      }}
    >
      <div
        style={{
          backgroundColor: multiplayerColorFromIndex(followedUser.colorIndex).background,
          color: multiplayerColorFromIndex(followedUser.colorIndex).foreground,
          padding: '2px 10px',
          borderRadius: 10,
          boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
        }}
      >
        You're following {followedUser.name}
      </div>
    </div>
  )
})
FollowingOverlay.displayName = 'FollowingOverlay'

const MultiplayerShadows = React.memo(() => {
  const myUserId = useMyUserId()
  const updateMyPresence = useUpdateMyPresence()

  const collabs = useStorage((store) => store.collaborators)
  const others = useOthers((list) => {
    if (myUserId == null) {
      return []
    }
    const presences = normalizeOthersList(myUserId, list)
    return presences.map((p) => ({
      presenceInfo: p,
      userInfo: collabs[p.id],
    }))
  })

  const shadows = React.useMemo(() => {
    return others.flatMap(
      (other) =>
        other.presenceInfo.presence.activeFrames?.map((activeFrame) => ({
          activeFrame: activeFrame,
          colorIndex: other.userInfo.colorIndex,
        })) ?? [],
    )
  }, [others])

  const myActiveFrames = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.activeFrames,
    'MultiplayerShadows activeFrames',
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'MultiplayerShadows canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'MultiplayerShadows canvasOffset',
  )

  const editorRef = useRefEditorState((store) => ({
    jsxMetadata: store.editor.jsxMetadata,
  }))

  useSelectorWithCallback(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession?.interactionData,
    (interactionData) => {
      if (interactionData?.type === 'DRAG') {
        updateMyPresence({
          activeFrames: mapDropNulls(({ target, action, source }): PresenceActiveFrame | null => {
            const { jsxMetadata } = editorRef.current
            switch (target.type) {
              case 'ACTIVE_FRAME_TARGET_RECT':
                return { frame: target.rect, action, source }
              case 'ACTIVE_FRAME_TARGET_PATH':
                const frame = MetadataUtils.getFrameInCanvasCoords(target.path, jsxMetadata)
                return { frame: zeroRectIfNullOrInfinity(frame), action, source }
              default:
                assertNever(target)
            }
          }, myActiveFrames),
        })
      } else {
        updateMyPresence({ activeFrames: [] })
      }
    },
    'MultiplayerShadows update presence shadows',
  )

  return (
    <>
      {shadows.map((shadow, index) => {
        const { frame, action, source } = shadow.activeFrame
        const color = multiplayerColorFromIndex(shadow.colorIndex)

        const framePosition = canvasPointToWindowPoint(frame, canvasScale, canvasOffset)
        const scaledFrame = scaleRect(frame, canvasScale)

        const sourcePosition = canvasPointToWindowPoint(source, canvasScale, canvasOffset)
        const scaledSource = scaleRect(source, canvasScale)

        return (
          <React.Fragment key={`shadow-${index}`}>
            <div
              style={{
                position: 'fixed',
                top: sourcePosition.y,
                left: sourcePosition.x,
                width: scaledSource.width,
                height: scaledSource.height,
                pointerEvents: 'none',
                border: `1px dashed ${color.background}`,
                opacity: 0.5,
              }}
            />
            <motion.div
              initial={{
                x: framePosition.x,
                y: framePosition.y,
                width: scaledFrame.width,
                height: scaledFrame.height,
              }}
              animate={{
                x: framePosition.x,
                y: framePosition.y,
                width: scaledFrame.width,
                height: scaledFrame.height,
              }}
              transition={{
                type: 'spring',
                damping: 30,
                mass: 0.8,
                stiffness: 350,
              }}
              style={{
                position: 'fixed',
                pointerEvents: 'none',
                background: `${color.background}44`,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                fontSize: 9,
                color: color.background,
                border: `1px dashed ${color.background}`,
              }}
            >
              {activeFrameActionToString(action)}
            </motion.div>
          </React.Fragment>
        )
      })}
    </>
  )
})
MultiplayerShadows.displayName = 'MultiplayerShadows'
