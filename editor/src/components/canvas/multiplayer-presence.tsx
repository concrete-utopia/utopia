import type { User } from '@liveblocks/client'
import { motion } from 'framer-motion'
import { useAtom } from 'jotai'
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
import { getCollaborator, useAddMyselfToCollaborators } from '../../core/commenting/comment-hooks'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import type { CanvasPoint } from '../../core/shared/math-utils'
import { pointsEqual, windowPoint, zeroRectIfNullOrInfinity } from '../../core/shared/math-utils'
import {
  isPlayerOnAnotherRemixRoute,
  multiplayerColorFromIndex,
  normalizeOthersList,
} from '../../core/shared/multiplayer'
import { assertNever } from '../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../utils/react-performance'
import { UtopiaStyles, UtopiaTheme, useColorTheme } from '../../uuiui'
import { notice } from '../common/notice'
import type { EditorAction } from '../editor/action-types'
import { isLoggedIn } from '../editor/action-types'
import { showToast, switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes, isFollowMode } from '../editor/editor-modes'
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
import { useRemixPresence } from '../../core/shared/multiplayer-hooks'
import { CanvasOffsetWrapper } from './controls/canvas-offset-wrapper'

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

  React.useEffect(() => {
    // when the room changes, reset
    dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
  }, [room.id, dispatch])

  if (!isLoggedIn(loginState)) {
    return null
  }

  return (
    <>
      <FollowingOverlay />
      <MultiplayerShadows />
      <MultiplayerCursors />
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
  const myRemixPresence = me.presence.remix ?? null

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
        const isOnAnotherRoute = isPlayerOnAnotherRemixRoute(
          myRemixPresence,
          other.presenceInfo.presence.remix ?? null,
        )
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
            opacity={isOnAnotherRoute ? 0.25 : 1}
          />
        )
      })}
    </div>
  )
})
MultiplayerCursors.displayName = 'MultiplayerCursors'

const MultiplayerCursor = React.memo(
  ({
    name,
    colorIndex,
    position,
    opacity,
  }: {
    name: string | null
    colorIndex: number | null
    position: CanvasPoint
    opacity: number
  }) => {
    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'MultiplayerCursor canvasScale',
    )
    const color = multiplayerColorFromIndex(colorIndex)

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
            color: color.foreground,
            backgroundColor: color.background,
            padding: '0 4px',
            borderRadius: 2,
            boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
            fontWeight: 'bold',
            fontSize: 9,
            left: 5,
            top: 5,
            position: 'fixed',
            pointerEvents: 'none',
            opacity: opacity,
            scale: canvasScale <= 1 ? 1 / canvasScale : 1,
          }}
        >
          {/* This is a temporary placeholder for a good pointer icon */}
          <div
            style={{
              width: 0,
              height: 0,
              borderTop: `5px solid transparent`,
              borderBottom: `5px solid transparent`,
              borderRight: `5px solid ${color.background}`,
              transform: 'rotate(45deg)',
              position: 'absolute',
              top: -3,
              left: -1,
              zoom: canvasScale > 1 ? 1 / canvasScale : 1,
            }}
          />
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
              left: 5,
              top: 5,
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
      return isFollowMode(mode) && other.id === mode.playerId
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

  const [, setActiveRemixScene] = useAtom(ActiveRemixSceneAtom)
  const [remixNavigationState, setRemixNavigationState] = useAtom(RemixNavigationAtom)

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
        const newNavigationState = { ...remixNavigationState }
        const sceneState = newNavigationState[presence.remix.scene]
        if (sceneState != null && presence.remix.locationRoute != null) {
          sceneState.location.pathname = presence.remix.locationRoute
          setActiveRemixScene(EP.fromString(presence.remix.scene))
          setRemixNavigationState(newNavigationState)
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
    [
      dispatch,
      canvasScale,
      canvasOffset,
      setActiveRemixScene,
      setRemixNavigationState,
      remixPresence,
      remixNavigationState,
    ],
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
          backgroundColor: colorTheme.primary.value,
          color: colorTheme.white.value,
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
  const me = useSelf()
  const updateMyPresence = useUpdateMyPresence()

  const collabs = useStorage((store) => store.collaborators)
  const others = useOthers((list) => {
    const presences = normalizeOthersList(me.id, list)
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
        const sourcePosition = canvasPointToWindowPoint(source, canvasScale, canvasOffset)
        return (
          <React.Fragment key={`shadow-${index}`}>
            <div
              style={{
                position: 'fixed',
                top: sourcePosition.y,
                left: sourcePosition.x,
                width: source.width,
                height: source.height,
                pointerEvents: 'none',
                border: `1px dashed ${color.background}`,
                opacity: 0.5,
              }}
            />
            <motion.div
              initial={{
                x: framePosition.x,
                y: framePosition.y,
                width: frame.width,
                height: frame.height,
              }}
              animate={{
                x: framePosition.x,
                y: framePosition.y,
                width: frame.width,
                height: frame.height,
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
