import type { User } from '@liveblocks/client'
import { motion } from 'framer-motion'
import React from 'react'
import {
  useOthers,
  useOthersListener,
  useRoom,
  useSelf,
  useStorage,
  useUpdateMyPresence,
} from '../../../liveblocks.config'
import type { Presence, UserMeta } from '../../../liveblocks.config'
import type { CanvasPoint } from '../../core/shared/math-utils'
import { pointsEqual, windowPoint } from '../../core/shared/math-utils'
import { multiplayerColorFromIndex, normalizeOthersList } from '../../core/shared/multiplayer'
import { assertNever } from '../../core/shared/utils'
import { UtopiaTheme, useColorTheme } from '../../uuiui'
import type { EditorAction } from '../editor/action-types'
import { isLoggedIn } from '../editor/action-types'
import { switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes, isFollowMode } from '../editor/editor-modes'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState } from '../editor/store/store-hook'
import CanvasActions from './canvas-actions'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import { useAddMyselfToCollaborators } from '../../core/commenting/comment-hooks'

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

  useAddMyselfToCollaborators()

  React.useEffect(() => {
    if (!isLoggedIn(loginState)) {
      return
    }
    // when the canvas is panned or zoomed, update the presence
    updateMyPresence({ canvasScale, canvasOffset })
  }, [canvasScale, canvasOffset, updateMyPresence, loginState])

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
      <MultiplayerCursors />
      <FollowingOverlay />
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
      userInfo: collabs[p.id],
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
  }: {
    name: string | null
    colorIndex: number | null
    position: CanvasPoint
  }) => {
    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'MultiplayerCursor canvasScale',
    )
    const canvasOffset = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.roundedCanvasOffset,
      'MultiplayerCursor canvasOffset',
    )
    const color = multiplayerColorFromIndex(colorIndex)
    const windowPosition = canvasPointToWindowPoint(position, canvasScale, canvasOffset)

    return (
      <motion.div
        initial={windowPosition}
        animate={windowPosition}
        transition={{
          type: 'spring',
          damping: 30,
          mass: 0.8,
          stiffness: 350,
        }}
        style={{
          position: 'fixed',
          pointerEvents: 'none',
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
          }}
        />
        <div
          style={{
            color: color.foreground,
            backgroundColor: color.background,
            padding: '0 4px',
            borderRadius: 2,
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            fontWeight: 'bold',
            fontSize: 9,
            position: 'absolute',
            left: 5,
            top: 5,
          }}
        >
          {name}
        </div>
      </motion.div>
    )
  },
)
MultiplayerCursor.displayName = 'MultiplayerCursor'

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

  const updateCanvasFromOtherPresence = React.useCallback(
    (presence: Presence) => {
      let actions: EditorAction[] = []
      if (presence.canvasScale != null && presence.canvasScale !== canvasScale) {
        actions.push(CanvasActions.zoom(presence.canvasScale, null))
      }
      if (presence.canvasOffset != null && !pointsEqual(presence.canvasOffset, canvasOffset)) {
        actions.push(CanvasActions.positionCanvas(presence.canvasOffset))
      }
      if (actions.length > 0) {
        dispatch(actions)
      }
    },
    [dispatch, canvasScale, canvasOffset],
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
          boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        }}
      >
        You're following {followedUser.name}
      </div>
    </div>
  )
})
FollowingOverlay.displayName = 'FollowingOverlay'
