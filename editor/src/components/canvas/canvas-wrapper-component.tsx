import * as friendlyWords from 'friendly-words'
import React from 'react'
import { getProjectID } from '../../common/env-vars'
import { isLoginNotYetKnown } from '../../common/user'
import { usePubSubAtomWriteOnly } from '../../core/shared/atom-with-pub-sub'
import * as EP from '../../core/shared/element-path'
import type { ErrorMessage } from '../../core/shared/error-messages'
import { canvasPoint, windowPoint } from '../../core/shared/math-utils'
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
  UtopiaTheme,
  getPreferredColorScheme,
  useColorTheme,
} from '../../uuiui'
import { isLoggedIn } from '../editor/action-types'
import {
  clearPostActionData,
  setSafeMode,
  setSelectedComponents,
} from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  CanvasSizeAtom,
  createCanvasModelKILLME,
  getAllCodeEditorErrors,
} from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import CanvasActions from './canvas-actions'
import type { CanvasAction } from './canvas-types'
import { shouldShowErrorOverlay } from './canvas-utils'
import { FloatingPostActionMenu } from './controls/select-mode/post-action-menu'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import {
  Multiplayer,
  multiplayerCursorColors,
  type MultiplayerCursorColor,
  type MultiplayerState,
} from './multiplayer'
import type { OutboundMessage } from './multiplayer-messages'
import { messageMove } from './multiplayer-messages'

const testPlayerId = 'the-player-id'
const testRoomId = 'the-room'

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

  const anonymousPlayerName = React.useMemo(() => generateAnonymousUserName(), [])
  const [playerColors, setPlayerColors] = React.useState<{
    [id: string]: MultiplayerCursorColor
  }>({})

  const loginState = useEditorState(Substores.userState, (store) => store.userState.loginState, '')
  React.useEffect(() => {
    Multiplayer.state.loginState = loginState
  }, [loginState])

  const canvasScale = useEditorState(Substores.canvas, (store) => store.editor.canvas.scale, '')
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    '',
  )
  const [roomState, setRoomState] = React.useState<MultiplayerState | null>()
  const players = React.useMemo(() => {
    return roomState?.players ?? []
  }, [roomState])

  const listener = React.useCallback(
    (data: OutboundMessage) => {
      if (data.type === 'state') {
        setRoomState(data.state)
        setPlayerColors((current) => {
          const colors =
            getPreferredColorScheme() === 'dark'
              ? multiplayerCursorColors.dark
              : multiplayerCursorColors.light
          for (const player of data.state.players) {
            if (current[player.id] == null) {
              const color = colors[Math.floor(Math.random() * colors.length)]
              current[player.id] = color
            }
          }
          return current
        })
        if (data.state.selectedElements != null) {
          dispatch([setSelectedComponents(data.state.selectedElements.map(EP.fromString))])
        }
      } else if (data.type === 'move') {
        setRoomState((state) => {
          if (state == null) {
            return state
          }
          const newPlayers = [...state.players]
          const index = newPlayers.findIndex((p) => p.id === data.move.playerId)
          if (index >= 0) {
            newPlayers[index].position = data.move.position ?? newPlayers[index].position
            newPlayers[index].canvasOffset =
              data.move.canvasOffset ?? newPlayers[index].canvasOffset
            newPlayers[index].canvasScale = data.move.canvasScale ?? newPlayers[index].canvasScale
          }
          return { ...state, players: newPlayers }
        })
      }
    },
    [dispatch],
  )

  Multiplayer.listen('canvas', listener)

  const [following, setFollowing] = React.useState<string | null>(null)

  React.useEffect(() => {
    if (following == null) {
      return
    }
    if (roomState == null) {
      return
    }
    const target = roomState.players.find((p) => p.id === following)
    if (target == null) {
      return
    }
    const actions: CanvasAction[] = []
    if (target.canvasScale != null && target.canvasScale !== canvasScale) {
      actions.push(CanvasActions.zoom(target.canvasScale, null))
    }
    if (
      target.canvasOffset != null &&
      (target.canvasOffset.x !== canvasOffset.x || target.canvasOffset.y !== canvasOffset.y)
    ) {
      actions.push(CanvasActions.positionCanvas(target.canvasOffset))
    }
    dispatch(actions)
  }, [roomState, following, dispatch, canvasScale, canvasOffset])

  const projectId = useEditorState(Substores.restOfEditor, (store) => store.editor.id, '')

  React.useEffect(() => {
    if (projectId == null || isLoginNotYetKnown(loginState)) {
      return
    }
    const id = testPlayerId //isLoggedIn(loginState) ? loginState.user.userId : anonymousPlayerName
    const name = testPlayerId
    Multiplayer.state.playerId = id
    Multiplayer.state.playerName = name
    const previousRoom = Multiplayer.state.roomId
    Multiplayer.state.roomId = testRoomId // TODO projectId
    if (previousRoom != null && previousRoom !== testRoomId /* TODO projectId */) {
      Multiplayer.reset()
    }
  }, [loginState, anonymousPlayerName, projectId])

  const onMouseMove = React.useCallback(
    (e: React.MouseEvent) => {
      Multiplayer.send(
        messageMove({
          position: windowToCanvasCoordinates(
            canvasScale,
            canvasOffset,
            windowPoint({ x: e.clientX, y: e.clientY }),
          ).canvasPositionRounded,
          canvasOffset: canvasOffset,
          canvasScale: canvasScale,
        }),
      )
    },
    [canvasScale, canvasOffset],
  )

  const colorTheme = useColorTheme()

  const onClickFollow = React.useCallback(
    (id: string) => () => {
      const newFollowing = following === id ? null : id
      Multiplayer.state.following = newFollowing != null
      setFollowing(newFollowing)
    },
    [following],
  )

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
      onMouseMove={onMouseMove}
    >
      {players.length > 0 && roomState != null && (
        <div
          style={{
            position: 'fixed',
            zIndex: 1000,
            bottom: 5,
            right: 5,
            boxShadow: `0px 0px 3px ${colorTheme.panelShadowColor.value}`,
            padding: 4,
            fontSize: 10,
            display: 'flex',
            flexDirection: 'column',
            gap: 4,
            backgroundColor: colorTheme.bg0.value,
            color: colorTheme.fg0.value,
          }}
        >
          {roomState.players
            .sort((a, b) => -a.id.localeCompare(b.id))
            .map((p) => {
              const isSelf = p.id === Multiplayer.state.playerId
              return (
                <div
                  key={p.id}
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
                      backgroundColor: isSelf
                        ? 'transparent'
                        : playerColors[p.id]?.background ?? '#000',
                      width: 6,
                      height: 6,
                    }}
                  />
                  <div style={{ flex: 1 }}>{p.name}</div>
                  {!isSelf ? (
                    <Button
                      spotlight
                      highlight
                      style={{ padding: '0 4px' }}
                      onClick={onClickFollow(p.id)}
                    >
                      {following !== p.id ? 'Follow' : 'Stop following'}
                    </Button>
                  ) : (
                    <div>(it's you!)</div>
                  )}
                </div>
              )
            })}
        </div>
      )}
      {roomState != null &&
        roomState.players.map((p) => {
          if (p.id == Multiplayer.state.playerId || p.position == null) {
            return null
          }
          const pos = canvasPointToWindowPoint(p.position, canvasScale, canvasOffset)
          return (
            <div
              key={p.id}
              style={{
                position: 'fixed',
                left: pos.x,
                top: pos.y,
                zIndex: 1,
                pointerEvents: 'none',
              }}
            >
              <div style={{ position: 'relative' }}>
                <svg
                  width='36px'
                  height='36px'
                  viewBox='0 0 36 36'
                  version='1.1'
                  xmlns='http://www.w3.org/2000/svg'
                  xmlnsXlink='http://www.w3.org/1999/xlink'
                  style={{ position: 'absolute', left: -18, top: -26, transform: 'rotate(-5deg)' }}
                >
                  <defs>
                    <polygon id='path-1' points='18 18 18 33 21.75 28 28 28'></polygon>
                    <filter
                      x='-95.0%'
                      y='-50.0%'
                      width='290.0%'
                      height='226.7%'
                      filterUnits='objectBoundingBox'
                      id='filter-2'
                    >
                      <feMorphology
                        radius='1'
                        operator='dilate'
                        in='SourceAlpha'
                        result='shadowSpreadOuter1'
                      ></feMorphology>
                      <feOffset
                        dx='0'
                        dy='2'
                        in='shadowSpreadOuter1'
                        result='shadowOffsetOuter1'
                      ></feOffset>
                      <feGaussianBlur
                        stdDeviation='2.5'
                        in='shadowOffsetOuter1'
                        result='shadowBlurOuter1'
                      ></feGaussianBlur>
                      <feComposite
                        in='shadowBlurOuter1'
                        in2='SourceAlpha'
                        operator='out'
                        result='shadowBlurOuter1'
                      ></feComposite>
                      <feColorMatrix
                        values='0 0 0 0 0.632493622   0 0 0 0 0.632493622   0 0 0 0 0.632493622  0 0 0 1 0'
                        type='matrix'
                        in='shadowBlurOuter1'
                      ></feColorMatrix>
                    </filter>
                  </defs>
                  <g id='Page-1' stroke='none' strokeWidth='1' fill='none' fillRule='evenodd'>
                    <g id='cursor-default'>
                      <g id='Default'>
                        <use
                          fill='black'
                          fillOpacity='1'
                          filter='url(#filter-2)'
                          xlinkHref='#path-1'
                        ></use>
                        <use
                          fill={playerColors[p.id]?.background ?? '#000'}
                          fillRule='evenodd'
                          xlinkHref='#path-1'
                        ></use>
                        <path
                          stroke={'#fff'}
                          strokeWidth='.5'
                          d='M22,28.5 L17.5,34.5 L17.5,16.7928932 L29.2071068,28.5 L22,28.5 Z'
                        ></path>
                      </g>
                    </g>
                  </g>
                </svg>
                <div
                  style={{
                    position: 'absolute',
                    backgroundColor: playerColors[p.id]?.background ?? '#000',
                    color: playerColors[p.id]?.foreground ?? '#fff',
                    borderRadius: 4,
                    fontSize: 10,
                    fontWeight: 'bold',
                    padding: '0px 6px',
                    top: 4,
                    left: 4,
                  }}
                >
                  {p.name ?? p.id}
                </div>
              </div>
            </div>
          )
        })}
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
