import * as EP from '../../core/shared/element-path'
import type { CanvasPoint, CanvasVector } from '../../core/shared/math-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import { generateUUID } from '../../utils/utils'
import { Multiplayer, type MultiplayerState as RoomState } from './multiplayer'

function newMessageId(): string {
  return `${Multiplayer.state.roomId}.${Multiplayer.state.playerId}.${generateUUID()}`
}

type BaseMessage = {
  id: string
  reply: boolean
}

export type Message = MessageSelection | MessageMove | MessageHandshake

type MessageSelection = BaseMessage & {
  type: 'selection'
  selection: {
    paths: string[]
  }
}

export function messageSelection(paths: ElementPath[]): MessageSelection {
  return {
    id: newMessageId(),
    reply: false,
    type: 'selection',
    selection: {
      paths: paths.map(EP.toString),
    },
  }
}

type MoveData = {
  playerId: string | null
  position: CanvasPoint | null
  canvasOffset: CanvasVector | null
  canvasScale: number | null
}

type MessageMove = BaseMessage & {
  type: 'move'
  reply: boolean
  move: MoveData
}

export function messageMove(params: {
  position?: CanvasPoint
  canvasOffset?: CanvasVector
  canvasScale?: number
}): MessageMove {
  return {
    id: newMessageId(),
    reply: false,
    type: 'move',
    move: {
      playerId: null,
      position: params.position ?? null,
      canvasOffset: params.canvasOffset ?? null,
      canvasScale: params.canvasScale ?? null,
    },
  }
}

type MessageHandshake = BaseMessage & {
  type: 'handshake'
  handshake: {
    playerId: string
    playerName: string
    roomId: string
  }
}

export function messageHandshake(
  playerId: string,
  playerName: string,
  roomId: string,
): MessageHandshake {
  return {
    id: newMessageId(),
    reply: true,
    type: 'handshake',
    handshake: {
      playerId: playerId,
      playerName: playerName,
      roomId: roomId,
    },
  }
}

type MessagePing = BaseMessage & {
  type: 'ping'
}

export function messagePing(): MessagePing {
  return {
    id: newMessageId(),
    reply: false,
    type: 'ping',
  }
}

export type OutboundMessage = OutboundMessageRoomState | OutboundMessageMove

type reply = {
  requestId: string
  ok: boolean
}

type BaseOutboundMessage = {
  reply: reply | null
}

export type OutboundMessageRoomState = BaseOutboundMessage & {
  type: 'state'
  state: RoomState
}

export type OutboundMessageMove = BaseOutboundMessage & {
  type: 'move'
  move: MoveData
}
