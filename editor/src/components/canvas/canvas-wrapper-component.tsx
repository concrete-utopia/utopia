import * as friendlyWords from 'friendly-words'
import React from 'react'
import { usePubSubAtomWriteOnly } from '../../core/shared/atom-with-pub-sub'
import type { ErrorMessage } from '../../core/shared/error-messages'
import { useErrorOverlayRecords } from '../../core/shared/runtime-report-logs'
import { NO_OP, fastForEach } from '../../core/shared/utils'
import { EditorCanvas } from '../../templates/editor-canvas'
import CloseButton from '../../third-party/react-error-overlay/components/CloseButton'
import ErrorOverlay from '../../third-party/react-error-overlay/components/ErrorOverlay'
import Footer from '../../third-party/react-error-overlay/components/Footer'
import Header from '../../third-party/react-error-overlay/components/Header'
import {
  Button,
  FlexColumn,
  FlexRow,
  Icons,
  SquareButton,
  UtopiaTheme,
  getPreferredColorScheme,
  useColorTheme,
} from '../../uuiui'
import { clearPostActionData, setSafeMode } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  CanvasSizeAtom,
  createCanvasModelKILLME,
  getAllCodeEditorErrors,
} from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { shouldShowErrorOverlay } from './canvas-utils'
import { FloatingPostActionMenu } from './controls/select-mode/post-action-menu'
import {
  liveblocksThrottle,
  useMyPresence,
  useOthers,
  useRoom,
  useSelf,
} from '../../../liveblocks.config'
import { ClientSideSuspense } from '@liveblocks/react'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import { isLoginNotYetKnown } from '../../common/user'
import { windowPoint, canvasPoint } from '../../core/shared/math-utils'
import { isLoggedIn } from '../editor/action-types'
import CanvasActions from './canvas-actions'
import type { CanvasAction } from './canvas-types'
import { multiplayerCursorColors } from './multiplayer'
import { MultiplayerCursor } from './multiplayer-cursors'
import { when } from '../../utils/react-conditionals'

export function filterOldPasses(errorMessages: Array<ErrorMessage>): Array<ErrorMessage> {
  let passTimes: { [key: string]: number } = {}
  fastForEach(errorMessages, (errorMessage) => {
    if (errorMessage.passTime != null) {
      if (errorMessage.source in passTimes) {
        const existingPassCount = passTimes[errorMessage.source]
        if (errorMessage.passTime > existingPassCount) {
          passTimes[errorMessage.source] = errorMessage.passTime
        }
      } else {
        passTimes[errorMessage.source] = errorMessage.passTime
      }
    }
  })
  return errorMessages.filter((errorMessage) => {
    if (errorMessage.passTime == null) {
      return true
    } else {
      return passTimes[errorMessage.source] === errorMessage.passTime
    }
  })
}

function generateAnonymousUserName(): string {
  const suffix = friendlyWords.objects[Math.floor(Math.random() * friendlyWords.objects.length)]
  return 'Anonymous' + ' ' + suffix
}

export const CanvasWrapperComponent = React.memo(() => {
  const dispatch = useDispatch()
  const { editorState, derivedState, userState } = useEditorState(
    Substores.fullStore,
    (store) => ({
      editorState: store.editor,
      derivedState: store.derived,
      userState: store.userState,
    }),
    'CanvasWrapperComponent',
  )

  const builtinDependencies = useEditorState(
    Substores.builtInDependencies,
    (store) => store.builtInDependencies,
    'CanvasWrapperComponent builtinDependencies',
  )

  const fatalErrors = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return getAllCodeEditorErrors(store.editor.codeEditorErrors, 'fatal', true)
    },
    'CanvasWrapperComponent fatalErrors',
  )

  const safeMode = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.safeMode
    },
    'CanvasWrapperComponent safeMode',
  )

  const scale = useEditorState(Substores.canvas, (store) => store.editor.canvas.scale, 'scale')

  const updateCanvasSize = usePubSubAtomWriteOnly(CanvasSizeAtom)

  const postActionSessionInProgress = useEditorState(
    Substores.postActionInteractionSession,
    (store) => store.postActionInteractionSession != null,
    'CanvasWrapperComponent postActionSessionInProgress',
  )
  const { errorRecords, overlayErrors } = useErrorOverlayRecords()
  const errorOverlayShown = shouldShowErrorOverlay(errorRecords, overlayErrors)
  const shouldDimErrorMessage = postActionSessionInProgress && errorOverlayShown

  const onOverlayClick = React.useCallback(() => {
    if (shouldDimErrorMessage) {
      dispatch([clearPostActionData()])
    }
  }, [dispatch, shouldDimErrorMessage])

  return (
    <FlexColumn
      className='CanvasWrapperComponent'
      style={{
        position: 'relative',
        overflowX: 'hidden',
        justifyContent: 'stretch',
        alignItems: 'stretch',
        flexGrow: 1,
        height: '100%',
        // ^ prevents Monaco from pushing the inspector out
      }}
    >
      <ClientSideSuspense fallback={<div>Loading…</div>}>{() => <Room />}</ClientSideSuspense>

      {fatalErrors.length === 0 && !safeMode ? (
        <EditorCanvas
          userState={userState}
          editor={editorState}
          derived={derivedState}
          model={createCanvasModelKILLME(editorState, derivedState)}
          builtinDependencies={builtinDependencies}
          updateCanvasSize={updateCanvasSize}
          dispatch={dispatch}
        />
      ) : null}

      <FlexRow
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          transform: 'translateZ(0)', // to keep this from tarnishing canvas render performance, we force it to a new layer
          pointerEvents: errorOverlayShown ? 'initial' : 'none', // you need to re-enable pointerevents for the various overlays
          transformOrigin: 'left top',
        }}
        onClick={onOverlayClick}
      >
        <div
          style={{
            position: 'relative',
            width: '100%',
            height: '100%',
            pointerEvents: 'none',
            zoom: `${scale * 100}%`,
            background: `rgba(255, 255, 255, ${shouldDimErrorMessage ? 0.5 : 0})`,
          }}
        >
          <FloatingPostActionMenu errorOverlayShown={errorOverlayShown} />
        </div>
      </FlexRow>
    </FlexColumn>
  )
})

const Room = React.memo(() => {
  const [, setMyPresence] = useMyPresence()
  const self = useSelf()
  const connectionId = React.useMemo(() => self.connectionId, [self])
  const others = useOthers()

  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'Room loginState',
  )

  const canvasScale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'Room canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'Room canvasOffset',
  )

  const [following, setFollowing] = React.useState<string | null>(null)

  React.useEffect(() => {
    const anonymousPlayerName = `${generateAnonymousUserName()}`
    const anonymousPlayerId = `anonymous.${connectionId}`

    if (isLoginNotYetKnown(loginState)) {
      return
    }
    const playerId = isLoggedIn(loginState) ? loginState.user.userId : anonymousPlayerId
    const playerName = (
      isLoggedIn(loginState) ? loginState.user.name ?? playerId : anonymousPlayerName
    ).trim()

    const colorIndex = Math.floor(Math.random() * multiplayerCursorColors.dark.length)

    setMyPresence({
      playerId: playerId,
      playerName: playerName,
      color: {
        dark: multiplayerCursorColors.dark[colorIndex],
        light: multiplayerCursorColors.light[colorIndex],
      },
    })
  }, [loginState, setMyPresence, connectionId])

  React.useEffect(() => {
    function mousemove(e: MouseEvent) {
      const position = windowToCanvasCoordinates(
        canvasScale,
        canvasOffset,
        windowPoint({ x: e.clientX, y: e.clientY }),
      ).canvasPositionRounded
      setMyPresence({
        cursor: position,
        canvasOffset: canvasOffset,
        canvasScale: canvasScale,
      })
    }
    window.addEventListener('mousemove', mousemove)
    return function () {
      window.removeEventListener('mousemove', mousemove)
    }
  }, [canvasOffset, canvasScale, setMyPresence])

  const onClickFollow = React.useCallback(
    (id: string) => () => {
      const newFollowing = following === id ? null : id
      setFollowing(newFollowing)
    },
    [following],
  )

  const colorTheme = useColorTheme()

  const followTarget = React.useMemo(() => {
    if (following == null) {
      return null
    }
    const other = others.find((o) => o.presence.playerId === following)
    if (other == null) {
      return null
    }
    return other.presence
  }, [others, following])

  const dispatch = useDispatch()

  React.useEffect(() => {
    if (followTarget == null) {
      return
    }
    const actions: CanvasAction[] = []
    if (followTarget.canvasScale != null && followTarget.canvasScale !== canvasScale) {
      actions.push(CanvasActions.zoom(followTarget.canvasScale, null))
    }
    if (
      followTarget.canvasOffset != null &&
      (followTarget.canvasOffset.x !== canvasOffset.x ||
        followTarget.canvasOffset.y !== canvasOffset.y)
    ) {
      actions.push(CanvasActions.positionCanvas(followTarget.canvasOffset))
    }
    dispatch(actions)
  }, [followTarget, dispatch, canvasOffset, canvasScale])

  const room = useRoom()

  const [playerListVisble, setPlayerListVisible] = React.useState(false)
  const togglePlayerList = React.useCallback(() => {
    setPlayerListVisible((visible) => !visible)
  }, [])

  return (
    <>
      {when(
        others.length > 0,
        <div
          style={{
            position: 'fixed',
            zIndex: 1000,
            bottom: 5,
            right: 5,
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            padding: 4,
            minWidth: 100,
            fontSize: 10,
            display: 'flex',
            flexDirection: 'column',
            gap: 4,
            backgroundColor: colorTheme.bg0.value,
            color: colorTheme.fg0.value,
          }}
        >
          <div
            style={{
              fontSize: 10,
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              gap: 4,
            }}
          >
            <div style={{ flex: 1 }}>
              Online: <strong>{others.length + 1}</strong>
            </div>
            <div
              style={{
                display: 'flex',
                gap: 4,
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              {when(
                playerListVisble,
                <span style={{ fontSize: 8, fontFamily: 'monospace', color: colorTheme.fg7.value }}>
                  {room.id}
                </span>,
              )}
              <SquareButton highlight spotlight onClick={togglePlayerList}>
                <Icons.ExpansionArrow
                  style={{ transform: playerListVisble ? undefined : 'rotate(180deg)' }}
                />
              </SquareButton>
            </div>
          </div>
          {when(
            playerListVisble,
            others.map((other) => {
              const color =
                getPreferredColorScheme() === 'dark'
                  ? other.presence.color?.dark
                  : other.presence.color?.light
              const otherId = other.presence.playerId ?? `${other.connectionId}`
              return (
                <div
                  key={`player-${other.connectionId}`}
                  style={{
                    display: 'flex',
                    alignItems: 'center',
                    gap: 8,
                    border: `1px solid ${colorTheme.border0.value}`,
                    padding: 4,
                  }}
                >
                  <div
                    style={{
                      borderRadius: '100%',
                      backgroundColor: color?.background ?? '#000',
                      width: 6,
                      height: 6,
                    }}
                  />
                  <div style={{ flex: 1 }}>
                    {other.presence.playerName ?? other.presence.playerId ?? other.connectionId}
                  </div>
                  <Button
                    spotlight
                    highlight
                    style={{ padding: '0 4px' }}
                    onClick={onClickFollow(otherId)}
                  >
                    {following !== otherId ? 'Follow' : 'Stop following'}
                  </Button>
                </div>
              )
            }),
          )}
        </div>,
      )}
      {others.map((other) => {
        if (other.presence.cursor == null) {
          return null
        }
        const position = canvasPointToWindowPoint(
          canvasPoint(other.presence.cursor),
          canvasScale,
          canvasOffset,
        )
        const color =
          getPreferredColorScheme() === 'dark'
            ? other.presence.color?.dark
            : other.presence.color?.light
        return (
          <div
            key={`cursor-${other.connectionId}`}
            style={{
              position: 'fixed',
              zIndex: 1,
              pointerEvents: 'none',
              transform: `translate(${position.x + 1}px, ${position.y + 4}px)`,
              transition: `transform ${liveblocksThrottle}ms linear`,
            }}
          >
            <div style={{ position: 'relative', border: '1px solid black' }}>
              <MultiplayerCursor color={color ?? null} />
              <div
                style={{
                  position: 'absolute',
                  backgroundColor: color?.background ?? '#000',
                  color: color?.foreground ?? '#fff',
                  borderRadius: 4,
                  fontSize: 10,
                  fontWeight: 'bold',
                  padding: '0px 6px',
                  top: 8,
                  left: 1,
                  border: '1px solid #fff',
                  boxShadow: UtopiaTheme.panelStyles.shadows.medium,
                }}
              >
                {other.presence.playerName ?? other.presence.playerId ?? other.connectionId}
              </div>
            </div>
          </div>
        )
      })}
    </>
  )
})

Room.displayName = 'Room'

export const SafeModeErrorOverlay = React.memo(() => {
  const dispatch = useDispatch()
  const onTryAgain = React.useCallback(() => {
    dispatch([setSafeMode(false)], 'everyone')
  }, [dispatch])

  const wrapperStyle = {
    display: 'flex',
    flexDirection: 'column',
  } as const

  return (
    <ErrorOverlay shortcutHandler={NO_OP}>
      <CloseButton close={NO_OP} />
      <div style={wrapperStyle}>
        <Header headerText={'Recovered from crash'} />
        <div style={{ fontSize: '1.17em', fontWeight: 'bold' }}>
          We recovered your code after a serious crash.
        </div>
      </div>

      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
        }}
      >
        <div
          style={{ display: 'flex', minWidth: 120, justifyContent: 'center', alignItems: 'center' }}
        >
          <Button
            primary={true}
            outline={false}
            highlight
            onClick={onTryAgain}
            style={{
              paddingLeft: 4,
              paddingRight: 4,
              width: 80,
              cursor: 'pointer',
              height: UtopiaTheme.layout.inputHeight.default,
            }}
          >
            Try again
          </Button>
        </div>
        <Footer
          line1='Warning: If the problem is not fixed it will cause another crash'
          line2='If your code is safe and you can reload the editor to try again.'
        />
      </div>
    </ErrorOverlay>
  )
})
