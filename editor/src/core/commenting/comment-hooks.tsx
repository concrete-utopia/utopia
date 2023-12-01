import React from 'react'
import type { User } from '@liveblocks/client'
import { LiveObject, type ThreadData } from '@liveblocks/client'
import type { Presence, ThreadMetadata, UserMeta } from '../../../liveblocks.config'
import { useMutation, useSelf, useStorage, useThreads } from '../../../liveblocks.config'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'
import { normalizeMultiplayerName, possiblyUniqueColor } from '../shared/multiplayer'
import { isLoggedIn } from '../../common/user'
import type { CommentId, SceneCommentLocation } from '../../components/editor/editor-modes'
import { assertNever } from '../shared/utils'
import type { CanvasPoint } from '../shared/math-utils'
import {
  canvasPoint,
  getCanvasPointWithCanvasOffset,
  isNotNullFiniteRectangle,
  localPoint,
  offsetPoint,
  zeroCanvasPoint,
} from '../shared/math-utils'
import { MetadataUtils } from '../model/element-metadata-utils'
import { getIdOfScene } from '../../components/canvas/controls/comment-mode/comment-mode-hooks'

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

  const scenes = useScenesWithId()

  const location = React.useMemo(() => {
    switch (comment.type) {
      case 'new':
        switch (comment.location.type) {
          case 'canvas':
            return comment.location.position
          case 'scene':
            const scene = scenes.find(
              (s) => getIdOfScene(s) === (comment.location as SceneCommentLocation).sceneId,
            )

            if (scene == null || !isNotNullFiniteRectangle(scene.globalFrame)) {
              return getCanvasPointWithCanvasOffset(zeroCanvasPoint, comment.location.offset)
            }
            return getCanvasPointWithCanvasOffset(scene.globalFrame, comment.location.offset)
          default:
            assertNever(comment.location)
        }
        break
      case 'existing':
        if (thread == null) {
          return null
        }
        if (thread.metadata.sceneId == null) {
          return canvasPoint(thread.metadata)
        }
        const scene = scenes.find((s) => getIdOfScene(s) === thread.metadata.sceneId)

        if (scene == null) {
          return canvasPoint(thread.metadata)
        }
        if (!isNotNullFiniteRectangle(scene.globalFrame)) {
          return canvasPoint(thread.metadata)
        }
        return getCanvasPointWithCanvasOffset(scene.globalFrame, localPoint(thread.metadata))

      default:
        assertNever(comment)
    }
  }, [comment, thread, scenes])

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

export function useScenesWithId() {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const scenes = MetadataUtils.getScenesMetadata(store.editor.jsxMetadata)
      return scenes.filter((s) => getIdOfScene(s) != null)
    },
    'useScenesWithId scenes',
  )
}
