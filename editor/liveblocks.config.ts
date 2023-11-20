import { LiveObject } from '@liveblocks/client'
import { createClient } from '@liveblocks/client'
import { createRoomContext } from '@liveblocks/react'
import type { CanvasVector, WindowPoint } from './src/core/shared/math-utils'

export const liveblocksThrottle = 100 // ms

export const liveblocksClient = createClient({
  throttle: liveblocksThrottle,
  authEndpoint: '/v1/liveblocks/authentication',
})

// Presence represents the properties that exist on every user in the Room
// and that will automatically be kept in sync. Accessible through the
// `user.presence` property. Must be JSON-serializable.
export type Presence = {
  name: string | null
  cursor: WindowPoint | null
  canvasScale: number | null
  canvasOffset: CanvasVector | null
  colorIndex: number | null
  picture: string | null // TODO remove this once able to resolve users
}

export function initialPresence(): Presence {
  return {
    name: null,
    cursor: null,
    canvasScale: null,
    canvasOffset: null,
    colorIndex: null,
    picture: null,
  }
}

// Optionally, Storage represents the shared document that persists in the
// Room, even after all users leave. Fields under Storage typically are
// LiveList, LiveMap, LiveObject instances, for which updates are
// automatically persisted and synced to all connected clients.
export type Storage = {
  // author: LiveObject<{ firstName: string, lastName: string }>,
  // ...
  collaborators: LiveObject<{ [userId: string]: boolean }> // this is an object (and not a list) so we can quickly check if a user is a collaborator, but later we can extend the information by storing something more than a boolean (e.g. a permission level)
}

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
  // info?: Json,  // Accessible through `user.info`
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
  name: string // TODO: this is maybe unnecessary after we provide a resolveUsers function to liveblocks
  colorIndex: number
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

    // const usersData = await __fetchUsersFromDB__(userIds);
    //
    // return usersData.map((userData) => ({
    //   name: userData.name,
    //   avatar: userData.avatar.src,
    // }));

    return []
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
