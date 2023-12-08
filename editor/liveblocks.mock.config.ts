import type { createRoomContext } from '@liveblocks/react'
import type { ThreadData } from '@liveblocks/client'
import type {
  Presence,
  ThreadMetadata,
  UserMeta,
  Storage,
  RoomEvent,
  createRoomContextU,
} from './liveblocks.config'

type CreateRoomContextType = typeof createRoomContextU
type RoomContextType = ReturnType<CreateRoomContextType>

type UseThreads = RoomContextType['suspense']['useThreads']
type UseStorage = RoomContextType['suspense']['useStorage']
type UseCreateThread = RoomContextType['suspense']['useCreateThread']
type UseMutation = RoomContextType['suspense']['useMutation']

interface MockedLiveBlocksConfig {
  useThreads: UseThreads
  useStorage: UseStorage
  useCreateThread: UseCreateThread
  useMutation: UseMutation
}

const MOCKS: {
  threads: Array<ThreadData<ThreadMetadata>>
  storage: { collaborators: Record<string, UserMeta> }
} = {
  threads: [],
  storage: {
    collaborators: {},
  },
}

export const MOCK_LIVEBLOCKS_CONFIG: { current: MockedLiveBlocksConfig | null } = { current: null }

export function setMockData(
  threadsToSet: Array<ThreadData<ThreadMetadata>>,
  storageToSet: { collaborators: Record<string, UserMeta> },
) {
  MOCKS.threads = threadsToSet
  MOCKS.storage = storageToSet

  MOCK_LIVEBLOCKS_CONFIG.current = {
    useThreads: () => ({
      isLoading: false,
      threads: MOCKS.threads,
    }),
    useStorage: (cb) => cb(MOCKS.storage),
    useCreateThread:
      () =>
      ({ body, metadata }) => {
        const newThread: ThreadData<ThreadMetadata> = {
          type: 'thread',
          id: '33',
          roomId: 'mock-room-id',
          createdAt: new Date('05 October 2011 14:48 UTC').toISOString(),
          comments: [
            {
              type: 'comment',
              id: '44',
              threadId: '33',
              roomId: 'mock-room-id',
              userId: 'user-123',
              createdAt: new Date('05 October 2011 14:48 UTC').toISOString(),
              reactions: [],
              body: body,
            },
          ],
          metadata: metadata,
        }
        MOCKS.threads.push(newThread)
        return newThread
      },
    useMutation: () => (() => {}) as any,
  }
}
