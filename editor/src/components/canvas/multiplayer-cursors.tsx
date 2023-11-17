import { motion } from 'framer-motion'
import React from 'react'
import { useOthers, useRoom, useSelf, useUpdateMyPresence } from '../../../liveblocks.config'
import type { CanvasPoint } from '../../core/shared/math-utils'
import { pointsEqual, windowPoint } from '../../core/shared/math-utils'
import { UtopiaTheme, useColorTheme } from '../../uuiui'
import type { EditorAction } from '../editor/action-types'
import { isLoggedIn } from '../editor/action-types'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import {
  multiplayerColorFromIndex,
  normalizeMultiplayerName,
  normalizeOthersList,
  possiblyUniqueColor,
} from '../../core/shared/multiplayer'
import { useKeepShallowReferenceEquality } from '../../utils/react-performance'
import { useDispatch } from '../editor/store/dispatch-context'
import CanvasActions from './canvas-actions'
import { updateMultiplayerState } from '../editor/actions/action-creators'

export const MultiplayerPresence = React.memo(() => {
  const dispatch = useDispatch()

  const room = useRoom()
  const self = useSelf()
  const others = useOthers((list) => normalizeOthersList(self.id, list))
  const updateMyPresence = useUpdateMyPresence()

  const selfColorIndex = React.useMemo(() => self.presence.colorIndex, [self.presence])
  const otherColorIndices = useKeepShallowReferenceEquality(
    others.map((other) => other.presence.colorIndex),
  )

  const getColorIndex = React.useCallback(() => {
    return selfColorIndex ?? possiblyUniqueColor(otherColorIndices)
  }, [selfColorIndex, otherColorIndices])
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

  React.useEffect(() => {
    // when the login state changes, update the presence info
    if (!isLoggedIn(loginState)) {
      return
    }
    updateMyPresence({
      name: normalizeMultiplayerName(loginState.user.name ?? null),
      picture: loginState.user.picture ?? null, // TODO remove this once able to resolve users
      colorIndex: getColorIndex(),
    })
  }, [loginState, updateMyPresence, getColorIndex])

  React.useEffect(() => {
    if (!isLoggedIn(loginState)) {
      return
    }
    // when the canvas is panned or zoomed, update the presence
    updateMyPresence({ canvasScale, canvasOffset })
  }, [canvasScale, canvasOffset, updateMyPresence, loginState, mode])

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
    dispatch([updateMultiplayerState({ following: null })])
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
  const self = useSelf()
  const others = useOthers((list) => normalizeOthersList(self.id, list))

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
          other.presence.cursor == null ||
          other.presence.canvasOffset == null ||
          other.presence.canvasScale == null
        ) {
          return null
        }
        const position = windowToCanvasCoordinates(
          other.presence.canvasScale,
          other.presence.canvasOffset,
          other.presence.cursor,
        ).canvasPositionRounded
        return (
          <MultiplayerCursor
            key={`cursor-${other.id}`}
            name={other.presence.name}
            colorIndex={other.presence.colorIndex}
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

  const self = useSelf()
  const others = useOthers((list) => normalizeOthersList(self.id, list))

  const following = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.multiplayer.following,
    'FollowingOverlay following',
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
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'FollowingOverlay mode',
  )

  const followed = React.useMemo(() => {
    return others.find((other) => following != null && other.id === following.id)
  }, [others, following])

  React.useEffect(() => {
    // when following another player, apply its canvas constraints
    if (followed == null) {
      if (following != null) {
        // reset if the other player disconnects
        dispatch([updateMultiplayerState({ following: null })])
      }
      return
    }

    let actions: EditorAction[] = []
    if (followed.presence.canvasScale != null && followed.presence.canvasScale !== canvasScale) {
      actions.push(CanvasActions.zoom(followed.presence.canvasScale, null, true))
    }
    if (
      followed.presence.canvasOffset != null &&
      !pointsEqual(followed.presence.canvasOffset, canvasOffset)
    ) {
      actions.push(CanvasActions.positionCanvas(followed.presence.canvasOffset, true))
    }
    if (actions.length > 0) {
      dispatch(actions)
    }
  }, [followed, canvasScale, canvasOffset, dispatch, following, mode])

  if (followed == null) {
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
        You're following {followed.presence.name}
      </div>
    </div>
  )
})
FollowingOverlay.displayName = 'FollowingOverlay'
