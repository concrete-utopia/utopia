import { LiveObject, createClient } from '@liveblocks/client'
import { createRoomContext } from '@liveblocks/react'
import { getProjectID } from './src/common/env-vars'
import type { ActiveFrameAction } from './src/components/canvas/commands/set-active-frames-command'
import type { CanvasRectangle, CanvasVector, WindowPoint } from './src/core/shared/math-utils'
import type { RemixPresence } from './src/core/shared/multiplayer'
import { projectIdToRoomId } from './src/core/shared/multiplayer'

export const liveblocksThrottle = 100 // ms

export const liveblocksClient = createClient({
  throttle: liveblocksThrottle,
  authEndpoint: '/v1/liveblocks/authentication',
  unstable_fallbackToHTTP: true,
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

type SceneIdToRouteMapping = LiveObject<{ [sceneDataLabel: string]: string }>

// Optionally, Storage represents the shared document that persists in the
// Room, even after all users leave. Fields under Storage typically are
// LiveList, LiveMap, LiveObject instances, for which updates are
// automatically persisted and synced to all connected clients.
export type Storage = {
  // author: LiveObject<{ firstName: string, lastName: string }>,
  // ...
  collaborators: LiveObject<{ [userId: string]: User }> // this is an object (and not a list) so we can quickly check if a user is a collaborator, but later we can extend the information by storing something more than a boolean (e.g. a permission level)
  userReadStatusesByThread: LiveObject<{ [threadId: string]: UserReadStatuses }>
  remixSceneRoutes: LiveObject<{ [userId: string]: SceneIdToRouteMapping }>
}

export type User = LiveObject<UserMeta>

export type UserReadStatusesMeta = { [userId: string]: boolean }
export type UserReadStatuses = LiveObject<UserReadStatusesMeta>

export function initialStorage(): Storage {
  return {
    collaborators: new LiveObject(),
    userReadStatusesByThread: new LiveObject(),
    remixSceneRoutes: new LiveObject(),
  }
}

// Optionally, UserMeta represents static/readonly metadata on each user, as
// provided by your own custom auth back end (if used). Useful for data that
// will not change during a session, like a user's name or avatar.
export type UserMeta = {
  id: string // Accessible through `user.id`
  name: string | null
  avatar: string | null
  colorIndex: number | null
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
  // quote: string;
  // time: number;
  type: 'canvas'
  x: number // x and y is global when sceneId is undefined, and local to the scene when sceneId is not null
  y: number
  sceneId?: string
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
} = createRoomContext<Presence, Storage, UserMeta, RoomEvent, ThreadMetadata>(liveblocksClient, {
  async resolveUsers({ userIds }) {
    // Used only for Comments. Return a list of user information retrieved
    // from `userIds`. This info is used in comments, mentions etc.

    // This should be provided by the Utopia backend, but as a quick hack I store the user data in the room storage.
    // This means we need the room id to get the users, which is not provided to this function, but fortunately we can
    // recreate that from the project id.
    const projectId = getProjectID()
    if (projectId == null) {
      return []
    }

    const users = await getAllUsersFromRoom(projectIdToRoomId(projectId))
    return users.filter((u) => userIds.includes(u.id))
  },
  async resolveMentionSuggestions({ text, roomId }) {
    // Used only for Comments. Return a list of userIds where the name matches `text`.
    // These userIds are used to create a mention list when typing in the
    // composer.
    //
    // For example when you type "@jo", `text` will be `"jo"`, and
    // you should to return an array with John and Joanna's userIds.

    const users = await getAllUsersFromRoom(roomId)

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

async function getAllUsersFromRoom(roomId: string) {
  const room = liveblocksClient.getRoom(roomId)
  if (room == null) {
    return []
  }

  const storage = await room.getStorage()

  const collabs = storage.root.get('collaborators') as LiveObject<{ [userId: string]: User }>
  if (collabs == null) {
    return []
  }
  return Object.values(collabs.toObject()).map((u) => u.toObject())
}
