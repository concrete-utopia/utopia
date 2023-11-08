import { isLoginNotYetKnown } from '../../common/user'
import type { CanvasPoint, CanvasVector } from '../../core/shared/math-utils'
import type { LoginState } from '../editor/action-types'
import type { OutboundMessage } from './multiplayer-messages'
import { messageHandshake, messagePing } from './multiplayer-messages'

export type MultiplayerCursorColor = {
  background: string
  foreground: string
}

export const multiplayerCursorColors = {
  light: [
    { background: '#0092C0', foreground: 'white' },
    { background: '#128400', foreground: 'white' },
    { background: '#2A00D1', foreground: 'white' },
    { background: '#5C00D1', foreground: 'white' },
    { background: '#87C700', foreground: 'black' },
    { background: '#8C6D00', foreground: 'white' },
    { background: '#9C0000', foreground: 'white' },
    { background: '#B16CB7', foreground: 'white' },
    { background: '#B60479', foreground: 'white' },
    { background: '#E7B400', foreground: 'black' },
  ],
  dark: [
    { background: '#007DA4', foreground: 'white' },
    { background: '#19B500', foreground: 'black' },
    { background: '#3EFFF3', foreground: 'black' },
    { background: '#6842FF', foreground: 'white' },
    { background: '#C4FF46', foreground: 'black' },
    { background: '#CE00D3', foreground: 'white' },
    { background: '#DD0000', foreground: 'white' },
    { background: '#FF00A8', foreground: 'white' },
    { background: '#FFBA08', foreground: 'black' },
    { background: '#FFFF00', foreground: 'black' },
  ],
}

export type BroadcastMessage = {
  type: 'state'
  state: MultiplayerState
}

export type PlayerData = {
  id: string
  name: string
  position: CanvasPoint | null
  canvasOffset: CanvasVector | null
  canvasScale: number | null
}

export type MultiplayerState = {
  players: PlayerData[]
  selectedElements: string[] | null
}

let ws: WebSocket | null = null
let wsReady = false

type State = {
  handshakeId: string | null
  handshaken: boolean
  playerId: string | null
  playerName: string | null
  roomId: string | null
  loginState: LoginState | null
}

function newState(): State {
  return {
    handshakeId: null,
    handshaken: false,
    playerId: null,
    playerName: null,
    roomId: null,
    loginState: null,
  }
}

type ListenerCallback = (msg: OutboundMessage) => void
type Listeners = { [key: string]: ListenerCallback }

export const Multiplayer = {
  state: newState(),
  listeners: {} as Listeners,
  log: (...args: any[]) => {
    console.info('[multiplayer]', ...args)
  },
  send: (data: any) => {
    if (ws == null || !wsReady || !Multiplayer.state.handshaken) {
      return
    }
    if (ws.readyState === ws.OPEN) {
      ws.send(JSON.stringify(data))
    }
  },
  reset: () => {
    if (pingInterval != null) {
      clearInterval(pingInterval)
    }
    Multiplayer.log('resetting...')
    Multiplayer.state.handshaken = false
    initWebsocket()
  },
  handshake: () => {
    if (ws == null || Multiplayer.state.handshaken) {
      return
    }

    Multiplayer.log('trying handshake...')
    if (
      isLoginNotYetKnown(Multiplayer.state.loginState) ||
      Multiplayer.state.playerId == null ||
      Multiplayer.state.playerName == null ||
      Multiplayer.state.roomId == null
    ) {
      setTimeout(Multiplayer.handshake, 1000)
      return
    }

    const msg = messageHandshake(
      Multiplayer.state.playerId,
      Multiplayer.state.playerName,
      Multiplayer.state.roomId,
    )
    Multiplayer.state.handshakeId = msg.id

    Multiplayer.listen('handshake', (e) => {
      if (e.reply?.requestId === Multiplayer.state.handshakeId) {
        Multiplayer.unlisten('handshake')
        if (e.reply.ok) {
          Multiplayer.log('handshake ok!')
          Multiplayer.state.handshaken = true
        } else {
          setTimeout(Multiplayer.handshake, 1000)
        }
      }
    })

    startPing()

    ws.send(JSON.stringify(msg))
    Multiplayer.log('waiting for reply...')
  },
  listen: (name: string, cb: ListenerCallback) => {
    Multiplayer.listeners[name] = cb
  },
  unlisten: (name: string) => {
    delete Multiplayer.listeners[name]
  },
}

let pingInterval: NodeJS.Timer | null = null
function startPing() {
  pingInterval = setInterval(() => {
    Multiplayer.send(messagePing())
  }, 3_000)
}

function initWebsocket() {
  Multiplayer.log('init ws...')
  if (ws != null) {
    ws.close()
  }
  wsReady = false
  ws = new WebSocket('ws://localhost:8080')
  ws.onopen = () => {
    Multiplayer.log('ws open')
    wsReady = true
    Multiplayer.state.handshaken = false
    Multiplayer.handshake()
  }
  ws.onmessage = (e) => {
    for (const key of Object.keys(Multiplayer.listeners)) {
      const cb = Multiplayer.listeners[key]
      cb(JSON.parse(e.data))
    }
  }
  ws.onerror = (e) => {
    ws?.close()
    console.error('ws error', e)
  }
  ws.onclose = (e) => {
    console.warn('ws closed')
    setTimeout(initWebsocket, 3_000)
  }
}
initWebsocket()
