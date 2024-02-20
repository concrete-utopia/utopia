import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'
import { Method } from '../util/methods.server'
import { Collaborator } from '../types'

const BASE_URL = 'https://api.liveblocks.io/v2'

async function makeRequest<T>(method: Method, path: string): Promise<T> {
  const url = urlJoin(BASE_URL, path)
  const resp = await fetch(url, {
    method: method,
    headers: {
      Authorization: `Bearer ${ServerEnvironment.LiveblocksSecretKey}`,
    },
  })
  return resp.json()
}

function roomIdFromProjectId(projectId: string): string {
  return `project-room-${projectId}`
}

export interface RoomStorage {
  data: {
    collaborators: RoomCollaborators
  }
}

export interface RoomCollaborators {
  data: { [userId: string]: { data: Collaborator } }
}

async function getRoomStorage(projectId: string): Promise<RoomStorage> {
  const roomId = roomIdFromProjectId(projectId)
  return makeRequest('GET', `/rooms/${roomId}/storage`)
}

// REST API docs: https://liveblocks.io/docs/api-reference/rest-api-endpoints
export const LiveblocksAPI = {
  getRoomStorage,
}
