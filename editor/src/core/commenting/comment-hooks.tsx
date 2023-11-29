import React from 'react'
import type { User } from '@liveblocks/client'
import { LiveObject, type ThreadData } from '@liveblocks/client'
import type { Presence, ThreadMetadata, UserMeta } from '../../../liveblocks.config'
import { useMutation, useSelf, useStorage, useThreads } from '../../../liveblocks.config'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'
import { normalizeMultiplayerName, possiblyUniqueColor } from '../shared/multiplayer'
import { isLoggedIn } from '../../common/user'
import type { CommentId } from '../../components/editor/editor-modes'
import { assertNever } from '../shared/utils'
import type { CanvasPoint } from '../shared/math-utils'
import { canvasPoint } from '../shared/math-utils'

export function useCanvasCommentThreadAndLocation(comment: CommentId): {
  location: CanvasPoint | null
  thread: ThreadData<ThreadMetadata> | null
} {
  const { threads } = useThreads()

  const thread = React.useMemo(() => {
    switch (comment.type) {
      case 'new':
        return null
      case 'existing':
        return threads.find((t) => t.id === comment.threadId) ?? null
      default:
        assertNever(comment)
    }
  }, [threads, comment])

  const location = React.useMemo(() => {
    switch (comment.type) {
      case 'new':
        return comment.location
      case 'existing':
        if (thread == null) {
          return null
        }
        return canvasPoint(thread.metadata)
      default:
        assertNever(comment)
    }
  }, [comment, thread])

  return { location, thread }
}

function placeholderUserMeta(user: User<Presence, UserMeta>): UserMeta {
  return {
    id: user.id,
    name: null,
    avatar: null,
    colorIndex: null,
  }
}

interface Collaborators {
  [key: string]: UserMeta
}

export function getCollaborator(
  collabs: Collaborators,
  source: User<Presence, UserMeta>,
): UserMeta {
  return collabs[source.id] ?? placeholderUserMeta(source)
}

export function useMyUserAndPresence(): {
  presence: User<Presence, UserMeta>
  user: UserMeta
} {
  const me = useSelf()
  const collabs = useStorage((store) => store.collaborators)
  const myUser: UserMeta | null = getCollaborator(collabs, me)
  return {
    presence: me,
    user: myUser ?? placeholderUserMeta(me),
  }
}

export function useMyMultiplayerColorIndex() {
  const me = useMyUserAndPresence()
  return me.user.colorIndex
}

export function useAddMyselfToCollaborators() {
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'useAddMyselfToCollaborators loginState',
  )

  const addMyselfToCollaborators = useMutation(
    ({ storage, self }) => {
      if (!isLoggedIn(loginState)) {
        return
      }
      const collaborators = storage.get('collaborators')

      if (collaborators.get(self.id) == null) {
        const otherColorIndices = Object.values(collaborators.toObject()).map((u) =>
          u.get('colorIndex'),
        )

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

export function useIsOnAnotherRemixRoute(remixLocationRoute: string | null) {
  const me = useSelf()

  return (
    me.presence.remix?.locationRoute != null &&
    remixLocationRoute != null &&
    remixLocationRoute !== me.presence.remix.locationRoute
  )
}
