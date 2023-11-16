import { motion } from 'framer-motion'
import React from 'react'
import { useOthers, useSelf, useUpdateMyPresence } from '../../../liveblocks.config'
import type { CanvasPoint } from '../../core/shared/math-utils'
import { windowPoint } from '../../core/shared/math-utils'
import { UtopiaTheme } from '../../uuiui'
import { isLoggedIn } from '../editor/action-types'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import {
  multiplayerColorFromIndex,
  multiplayerColors,
  normalizeMultiplayerName,
  normalizeOthersList,
} from '../../core/shared/multiplayer'

export const MultiplayerCursors = React.memo(() => {
  const self = useSelf()
  const others = useOthers((list) => normalizeOthersList(self.id, list))
  const updateMyPresence = useUpdateMyPresence()

  const myColorIndex = React.useMemo(() => {
    return self.presence.colorIndex ?? Math.floor(Math.random() * multiplayerColors.light.length)
  }, [self.presence])

  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'MultiplayerCursors loginState',
  )
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'MultiplayerCursors canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'MultiplayerCursors canvasOffset',
  )

  React.useEffect(() => {
    if (!isLoggedIn(loginState)) {
      return
    }
    updateMyPresence({
      name: normalizeMultiplayerName(loginState.user.name ?? null),
      colorIndex: myColorIndex,
      picture: loginState.user.picture ?? null,
    })
  }, [loginState, updateMyPresence, myColorIndex])

  React.useEffect(() => {
    updateMyPresence({ canvasScale, canvasOffset })
  }, [canvasScale, canvasOffset, updateMyPresence])

  React.useEffect(() => {
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

  if (!isLoggedIn(loginState)) {
    return null
  }

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
