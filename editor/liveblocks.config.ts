import { LiveObject, createClient } from '@liveblocks/client'
import { createRoomContext } from '@liveblocks/react'
import { UTOPIA_BACKEND, getProjectID, isBackendBFF } from './src/common/env-vars'
import type { ActiveFrameAction } from './src/components/canvas/commands/set-active-frames-command'
import {
  type CanvasRectangle,
  type CanvasVector,
  type WindowPoint,
} from './src/core/shared/math-utils'
import type { RemixPresence } from './src/core/shared/multiplayer'
import { HEADERS } from './src/common/server'
import urljoin from 'url-join'
import { getCollaborators } from './src/components/editor/server'

export const liveblocksThrottle = 100 // ms

async function authCall(room?: string) {
  const resp = await fetch(urljoin(UTOPIA_BACKEND, 'liveblocks', 'authentication'), {
    credentials: 'include',
    method: 'POST',
    body: JSON.stringify({ room }),
    headers: HEADERS,
  })
  return resp.json()
}

export const liveblocksClient = createClient({
  throttle: liveblocksThrottle,
  authEndpoint: isBackendBFF() ? authCall : '/v1/liveblocks/authentication',
  unstable_fallbackToHTTP: true,
  resolveUsers: async ({ userIds }) => {
    // Used only for Comments. Return a list of user information retrieved
    // from `userIds`. This info is used in comments, mentions etc.

    const projectId = getProjectID()
    if (projectId == null) {
      return []
    }

    const users = await getCollaborators(projectId)
    return users.filter((u) => userIds.includes(u.id))
  },
  resolveMentionSuggestions: async ({ text }) => {
    // Used only for Comments. Return a list of userIds where the name matches `text`.
    // These userIds are used to create a mention list when typing in the
    // composer.
    //
    // For example when you type "@jo", `text` will be `"jo"`, and
    // you should to return an array with John and Joanna's userIds.

    const projectId = getProjectID()
    if (projectId == null) {
      return []
    }

    const users = await getCollaborators(projectId)

    if (text == null) {
      return users.map((u) => u.id)
    }

    // Otherwise, filter user names for the search `text` and return
    return users
      .filter((u) => {
        if (u.name == null) {
          return false
        }
        return u.name.toLowerCase().includes(text.toLowerCase())
      })
      .map((u) => u.id)
  },
})

// Presence represents the properties that exist on every user in the Room
// and that will automatically be kept in sync. Accessible through the
// `user.presence` property. Must be JSON-serializable.
export type Presence = {
  cursor: WindowPoint | null
  canvasScale: number | null
  canvasOffset: CanvasVector | null
  activeFrames?: PresenceActiveFrame[]
  following: string | null
  remix?: RemixPresence | null
}

export type PresenceActiveFrame = {
  action: ActiveFrameAction
  frame: CanvasRectangle
  source: CanvasRectangle
}

export function initialPresence(): Presence {
  return {
    cursor: null,
    canvasScale: null,
    canvasOffset: null,
    following: null,
  }
}

type SceneIdToRouteMapping = LiveObject<{ [sceneId: string]: string }>

// Optionally, Storage represents the shared document that persists in the
// Room, even after all users leave. Fields under Storage typically are
// LiveList, LiveMap, LiveObject instances, for which updates are
// automatically persisted and synced to all connected clients.
export type Storage = {
  // author: LiveObject<{ firstName: string, lastName: string }>,
  // ...
  collaborators: LiveObject<{ [userId: string]: User }> // TODO remove collaborators when the BFF is on
  userReadStatusesByThread: LiveObject<{ [threadId: string]: UserReadStatuses }>
  remixSceneRoutes: LiveObject<{ [userId: string]: SceneIdToRouteMapping }>
  connections: LiveObject<{ [userId: string]: ConnectionInfo[] }>
}

export type User = LiveObject<UserMeta>

export type UserReadStatusesMeta = { [userId: string]: boolean }
export type UserReadStatuses = LiveObject<UserReadStatusesMeta>

export function initialStorage(): Storage {
  return {
    collaborators: new LiveObject(), // TODO remove this when the BFF is on
    userReadStatusesByThread: new LiveObject(),
    remixSceneRoutes: new LiveObject(),
    connections: new LiveObject(),
  }
}

export type ConnectionInfo = {
  id: number
  startedAt: number
  lastSeen: number
  colorIndex: number | null
}

// Optionally, UserMeta represents static/readonly metadata on each user, as
// provided by your own custom auth back end (if used). Useful for data that
// will not change during a session, like a user's name or avatar.
export type UserMeta = {
  id: string // Accessible through `user.id`
  name: string | undefined
  avatar: string | undefined
}

// Optionally, the type of custom events broadcast and listened to in this
// room. Use a union for multiple events. Must be JSON-serializable.
export type ControlChangedRoomEvent = {
  type: 'CONTROL_CHANGED'
}
export type RoomEvent = ControlChangedRoomEvent

// Optionally, when using Comments, ThreadMetadata represents metadata on
// each thread. Can only contain booleans, strings, and numbers.

export type ThreadMetadata = {
  x: number
  y: number
  sceneId?: string
  sceneX?: number
  sceneY?: number
  remixLocationRoute?: string
  resolved: boolean
}

export const {
  suspense: {
    RoomProvider,
    useRoom,
    useMyPresence,
    useUpdateMyPresence,
    useSelf,
    useOthers,
    useOthersListener,
    useOthersMapped,
    useOthersConnectionIds,
    useOther,
    useBroadcastEvent,
    useEventListener,
    useErrorListener,
    useStorage,
    useObject,
    useMap,
    useList,
    useBatch,
    useHistory,
    useUndo,
    useRedo,
    useCanUndo,
    useCanRedo,
    useMutation,
    useStatus,
    useLostConnectionListener,
    useThreads,
    useUser,
    useCreateThread,
    useEditThreadMetadata,
    useCreateComment,
    useEditComment,
    useDeleteComment,
    useAddReaction,
  },
} = createRoomContext<Presence, Storage, UserMeta, RoomEvent, ThreadMetadata>(liveblocksClient, {})
