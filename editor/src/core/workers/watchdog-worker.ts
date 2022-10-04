import localforage from 'localforage'
import { getProjectLockedKey } from '../shared/utils'

export const DEFAULT_HEARTBEAT_TIMEOUT_MS = 2000
export const DEFAULT_HEARTBEAT_INTERVAL_MS = 5000

export type IncomingWatchdogWorkerMessage =
  | WatchdogInitMessage
  | WatchdogTerminateMessage
  | HeartbeatResponseMessage
export type OutgoingWatchdogWorkerMessage = HeartbeatRequestMessage | WatchdogInitResponseMessage

export interface WatchdogInitMessage {
  type: 'watchdoginit'
  projectId: string
  timeoutMs: number
  intervalMs: number
}

export interface WatchdogInitResponseMessage {
  type: 'watchdoginitresponse'
  setIntervalId: NodeJS.Timer
}

export interface WatchdogTerminateMessage {
  type: 'watchdogterminate'
  setIntervalId: NodeJS.Timer
}

export interface HeartbeatRequestMessage {
  type: 'heartbeatrequest'
  id: NodeJS.Timer
  projectId: string
}

export interface HeartbeatResponseMessage {
  type: 'heartbeatresponse'
  id: NodeJS.Timer
  projectId: string
  safeMode: boolean
}

export function createWatchdogInitMessage(
  projectId: string,
  timeoutMs: number,
  intervalMs: number,
): WatchdogInitMessage {
  return {
    type: 'watchdoginit',
    projectId: projectId,
    timeoutMs: timeoutMs,
    intervalMs: intervalMs,
  }
}

export function createWatchdogInitResponseMessage(
  setIntervalId: NodeJS.Timer,
): WatchdogInitResponseMessage {
  return {
    type: 'watchdoginitresponse',
    setIntervalId: setIntervalId,
  }
}

export function createWatchdogTerminateMessage(
  setIntervalId: NodeJS.Timer,
): WatchdogTerminateMessage {
  return {
    type: 'watchdogterminate',
    setIntervalId: setIntervalId,
  }
}

export function createHeartbeatRequestMessage(
  id: NodeJS.Timer,
  projectId: string,
): HeartbeatRequestMessage {
  return {
    type: 'heartbeatrequest',
    id: id,
    projectId: projectId,
  }
}

export function createHeartbeatResponseMessage(
  id: NodeJS.Timer,
  projectId: string,
  safeMode: boolean,
): HeartbeatResponseMessage {
  return {
    type: 'heartbeatresponse',
    id: id,
    projectId: projectId,
    safeMode: safeMode,
  }
}

export async function handleMessage(
  workerMessage: IncomingWatchdogWorkerMessage,
  sendMessage: (content: OutgoingWatchdogWorkerMessage) => void,
) {
  switch (workerMessage.type) {
    case 'watchdoginit':
      const requestHeartbeat = () => {
        const id = setTimeout(() => {
          void localforage.setItem(getProjectLockedKey(workerMessage.projectId), true)
        }, workerMessage.timeoutMs)
        sendMessage(createHeartbeatRequestMessage(id, workerMessage.projectId))
      }
      requestHeartbeat()
      const setIntervalId = setInterval(requestHeartbeat, workerMessage.intervalMs)
      sendMessage(createWatchdogInitResponseMessage(setIntervalId))
      break
    case 'heartbeatresponse':
      clearTimeout(workerMessage.id)
      if (!workerMessage.safeMode) {
        void localforage.setItem(getProjectLockedKey(workerMessage.projectId), false)
      }
      break
    case 'watchdogterminate':
      clearTimeout(workerMessage.setIntervalId)
      break
  }
}
