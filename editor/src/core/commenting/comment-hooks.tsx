import React from 'react'
import { LiveObject, type ThreadData } from '@liveblocks/client'
import type { ThreadMetadata } from '../../../liveblocks.config'
import { useMutation, useRoom, useSelf, useStorage, useThreads } from '../../../liveblocks.config'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'
import { normalizeMultiplayerName, possiblyUniqueColor } from '../shared/multiplayer'
import { isLoggedIn } from '../../common/user'

export function useCanvasCommentThread(x: number, y: number): ThreadData<ThreadMetadata> | null {
  const { threads } = useThreads()
  const thread = threads.find((t) => t.metadata.x === x && t.metadata.y === y) ?? null
  return thread
}

export function useMyMultiplayerColorIndex() {
  const me = useSelf()
  return me.presence.colorIndex
}

export function useAddMyselfToCollaborators() {
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'MultiplayerCursors loginState',
  )

  const addMyselfToCollaborators = useMutation(
    ({ storage, self }) => {
      if (!isLoggedIn(loginState)) {
        return
      }
      const collaborators = storage.get('collaborators')

      const otherColorIndices = Object.values(collaborators).map((u) => u.colorIndex)

      if (collaborators.get(self.id) == null) {
        collaborators.set(
          self.id,
          new LiveObject({
            id: loginState.user.userId,
            name: normalizeMultiplayerName(loginState.user.name ?? null),
            avatar: loginState.user.picture ?? null,
            colorIndex: possiblyUniqueColor(otherColorIndices),
          }),
        )
      }
    },
    [loginState],
  )

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
