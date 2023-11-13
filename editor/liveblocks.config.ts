import { createClient } from '@liveblocks/client'
import { createRoomContext } from '@liveblocks/react'
import type { MultiplayerCursorColor } from './src/components/canvas/multiplayer'
import type { CanvasVector } from './src/core/shared/math-utils'

export const liveblocksThrottle = 100 // ms

export const liveblocksClient = createClient({
  publicApiKey: 'pk_dev_JKA4v6OjxmJkgn49CmIafpJypWfPCUZz2TDUytEFcCpInUcUMH604iXIBYllN40k',
  throttle: liveblocksThrottle,
  // authEndpoint: "/api/auth",
})

// Presence represents the properties that exist on every user in the Room
// and that will automatically be kept in sync. Accessible through the
// `user.presence` property. Must be JSON-serializable.
export type Presence = {
  playerId: string | null
  playerName: string | null
  color: { light: MultiplayerCursorColor; dark: MultiplayerCursorColor } | null
  cursor: { x: number; y: number } | null
  canvasScale: number | null
  canvasOffset: CanvasVector | null
}

export function initialPresence(): Presence {
  return {
    cursor: null,
    playerId: null,
    playerName: null,
    color: null,
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
}

// Optionally, UserMeta represents static/readonly metadata on each user, as
// provided by your own custom auth back end (if used). Useful for data that
// will not change during a session, like a user's name or avatar.
export type UserMeta = {
  // id?: string,  // Accessible through `user.id`
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
}

export const {
  suspense: {
    RoomProvider,
    useRoom,
    useMyPresence,
    useUpdateMyPresence,
    useSelf,
    useOthers,
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
