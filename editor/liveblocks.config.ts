import { LiveObject } from '@liveblocks/client'
import { createClient } from '@liveblocks/client'
import { createRoomContext } from '@liveblocks/react'
import { getProjectID } from './src/common/env-vars'
import { projectIdToRoomId } from './src/core/shared/multiplayer'
import type { CanvasRectangle, CanvasVector, WindowPoint } from './src/core/shared/math-utils'
import type { ActiveFrameAction } from './src/components/canvas/commands/set-active-frames-command'

export const liveblocksThrottle = 100 // ms

export const liveblocksClient = createClient({
  throttle: liveblocksThrottle,
  authEndpoint: '/v1/liveblocks/authentication',
})

// Presence represents the properties that exist on every user in the Room
// and that will automatically be kept in sync. Accessible through the
// `user.presence` property. Must be JSON-serializable.
export type Presence = {
  cursor: WindowPoint | null
  canvasScale: number | null
  canvasOffset: CanvasVector | null
  shadows?: { action: ActiveFrameAction; frame: CanvasRectangle }[]
}

export function initialPresence(): Presence {
  return {
    cursor: null,
    canvasScale: null,
    canvasOffset: null,
  }
}

// Optionally, Storage represents the shared document that persists in the
// Room, even after all users leave. Fields under Storage typically are
// LiveList, LiveMap, LiveObject instances, for which updates are
// automatically persisted and synced to all connected clients.
export type Storage = {
  // author: LiveObject<{ firstName: string, lastName: string }>,
  // ...
  collaborators: LiveObject<{ [userId: string]: User }> // this is an object (and not a list) so we can quickly check if a user is a collaborator, but later we can extend the information by storing something more than a boolean (e.g. a permission level)
}

export type User = LiveObject<UserMeta>

export function initialStorage(): Storage {
  return {
    collaborators: new LiveObject(),
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
export type RoomEvent = {
  // type: "NOTIFICATION",
  // ...
}

// Optionally, when using Comments, ThreadMetadata represents metadata on
// each thread. Can only contain booleans, strings, and numbers.
export type ThreadMetadata = {
  // resolved: boolean;
  // quote: string;
  // time: number;
  type: 'canvas'
  x: number
  y: number
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

    const room = liveblocksClient.getRoom(projectIdToRoomId(projectId))
    if (room == null) {
      return []
    }

    const storage = await room.getStorage()

    const collabs = storage.root.get('collaborators') as LiveObject<{ [userId: string]: User }>
    if (collabs == null) {
      return []
    }

    const users = Object.values(collabs.toObject()).map((u) => u.toObject())
    return users.filter((u) => userIds.includes(u.id))
  },
  async resolveMentionSuggestions({ text, roomId }) {
    // Used only for Comments. Return a list of userIds that match `text`.
    // These userIds are used to create a mention list when typing in the
    // composer.
    //
    // For example when you type "@jo", `text` will be `"jo"`, and
    // you should to return an array with John and Joanna's userIds:
    // ["john@example.com", "joanna@example.com"]

    // const userIds = await __fetchAllUserIdsFromDB__(roomId);
    //
    // Return all userIds if no `text`
    // if (!text) {
    //   return userIds;
    // }
    //
    // Otherwise, filter userIds for the search `text` and return
    // return userIds.filter((userId) =>
    //   userId.toLowerCase().includes(text.toLowerCase())
    // );

    return []
  },
})
