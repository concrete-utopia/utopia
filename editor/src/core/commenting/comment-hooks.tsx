import React from 'react'
import type { ThreadData } from '@liveblocks/client'
import type { ThreadMetadata } from '../../../liveblocks.config'
import { useMutation, useSelf, useStorage, useThreads } from '../../../liveblocks.config'

export function useCanvasCommentThread(x: number, y: number): ThreadData<ThreadMetadata> | null {
  const { threads } = useThreads()
  const thread = threads.find((t) => t.metadata.x === x && t.metadata.y === y) ?? null
  return thread
}

export function useMyMultiplayerColorIndex() {
  const self = useSelf()
  return self.presence.colorIndex
}

export function useAddMyselfToCollaborators() {
  const addMyselfToCollaborators = useMutation(({ storage, self }) => {
    const collaborators = storage.get('collaborators')

    if (collaborators.get(self.id) !== true) {
      collaborators.set(self.id, true)
    }
  }, [])

  const collabs = useStorage((store) => store.collaborators)

  React.useEffect(() => {
    if (collabs != null) {
      addMyselfToCollaborators()
    }
  }, [addMyselfToCollaborators, collabs])
}

export function useCollaborators() {
  return useStorage((store) => store.collaborators)
}
