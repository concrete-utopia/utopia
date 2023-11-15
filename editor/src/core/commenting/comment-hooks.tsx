import type { ThreadData } from '@liveblocks/client'
import type { ThreadMetadata } from '../../../liveblocks.config'
import { useThreads } from '../../../liveblocks.config'

export function useCanvasCommentThread(x: number, y: number): ThreadData<ThreadMetadata> | null {
  const { threads } = useThreads()
  const thread = threads.find((t) => t.metadata.x === x && t.metadata.y === y) ?? null
  return thread
}
