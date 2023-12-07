import React from 'react'
import type { User } from '@liveblocks/client'
import { LiveObject, type ThreadData } from '@liveblocks/client'
import type { Presence, ThreadMetadata, UserMeta } from '../../../liveblocks.config'
import {
  parseUserReadStatuses,
  printUserReadStatuses,
  useEditThreadMetadata,
  useMutation,
  useSelf,
  useStorage,
  useThreads,
} from '../../../liveblocks.config'
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
  zeroCanvasPoint,
} from '../shared/math-utils'
import { MetadataUtils } from '../model/element-metadata-utils'
import { getIdOfScene } from '../../components/canvas/controls/comment-mode/comment-mode-hooks'
import type { ElementPath } from '../shared/project-file-types'
import type { ElementInstanceMetadata } from '../shared/element-template'

export function useCanvasCommentThreadAndLocation(comment: CommentId): {
  location: CanvasPoint | null
  thread: ThreadData<ThreadMetadata> | null
} {
  const threads = useActiveThreads()

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
  return getCollaboratorById(collabs, source.id) ?? placeholderUserMeta(source)
}

export function getCollaboratorById(collabs: Collaborators, id: string): UserMeta | null {
  return collabs[id] ?? null
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

export function useScenesWithId(): Array<ElementInstanceMetadata> {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const scenes = MetadataUtils.getScenesMetadata(store.editor.jsxMetadata)
      return scenes.filter((s) => getIdOfScene(s) != null)
    },
    'useScenesWithId scenes',
  )
}

export function useSceneWithId(sceneId: string | null): ElementInstanceMetadata | null {
  return useEditorState(
    Substores.metadata,
    (store) => {
      if (sceneId == null) {
        return null
      }
      const scenes = MetadataUtils.getScenesMetadata(store.editor.jsxMetadata)
      return scenes.find((s) => getIdOfScene(s) != sceneId) ?? null
    },
    'useSceneWithId scene',
  )
}

export function useCanvasLocationOfThread(thread: ThreadData<ThreadMetadata>): {
  location: CanvasPoint
  scene: ElementPath | null
} {
  const scenes = useScenesWithId()

  if (thread.metadata.sceneId == null) {
    return { location: canvasPoint(thread.metadata), scene: null }
  }
  const scene = scenes.find((s) => getIdOfScene(s) === thread.metadata.sceneId)

  if (scene == null || !isNotNullFiniteRectangle(scene.globalFrame)) {
    return { location: canvasPoint(thread.metadata), scene: null }
  }
  return {
    location: getCanvasPointWithCanvasOffset(scene.globalFrame, localPoint(thread.metadata)),
    scene: scene.elementPath,
  }
}

export function useResolveThread() {
  const editThreadMetadata = useEditThreadMetadata()

  const resolveThread = React.useCallback(
    (thread: ThreadData<ThreadMetadata>) => {
      editThreadMetadata({ threadId: thread.id, metadata: { resolved: !thread.metadata.resolved } })
    },
    [editThreadMetadata],
  )

  return resolveThread
}

export function useActiveThreads() {
  const { threads: unresolvedThreads } = useUnresolvedThreads()
  const { threads: resolvedThreads } = useResolvedThreads()
  const showResolved = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.showResolvedThreads,
    'useActiveThreads showResolved',
  )
  if (!showResolved) {
    return unresolvedThreads
  }
  return [...unresolvedThreads, ...resolvedThreads]
}

export function useResolvedThreads() {
  const threads = useThreads()
  return {
    ...threads,
    threads: threads.threads.filter((t) => t.metadata.resolved === true),
  }
}

export function useUnresolvedThreads() {
  const threads = useThreads()
  return {
    ...threads,
    threads: threads.threads.filter((t) => t.metadata.resolved !== true),
  }
}

export function useSetCommentThreadReadStatusOnMount(thread: ThreadData<ThreadMetadata> | null) {
  const self = useSelf()
  const editThreadMetadata = useEditThreadMetadata()

  React.useEffect(() => {
    if (thread == null) {
      return
    }

    setCommentThreadReadStatus(thread, self.id, 'read', editThreadMetadata, 'keep-others')
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []) // only run it once on mount, because opening the popup means reading the thread, no dependencies added
}

export function useMyCommentThreadReadStatus(
  thread: ThreadData<ThreadMetadata> | null,
): CommentThreadReadStatus {
  const self = useSelf()
  if (thread == null) {
    return 'unread'
  }
  return getCommentThreadReadStatus(thread, self.id)
}

export type CommentThreadReadStatus = 'read' | 'unread'

function getCommentThreadReadStatus(
  thread: ThreadData<ThreadMetadata>,
  userId: string,
): CommentThreadReadStatus {
  const userReadStatuses = parseUserReadStatuses(thread.metadata.userReadStatuses)
  return userReadStatuses[userId] === true ? 'read' : 'unread'
}

export function setCommentThreadReadStatus(
  thread: ThreadData<ThreadMetadata>,
  userId: string,
  status: CommentThreadReadStatus,
  editThreadMetadata: ReturnType<typeof useEditThreadMetadata>,
  deleteOthers: 'delete-others' | 'keep-others',
) {
  const userReadStatuses =
    deleteOthers === 'delete-others' ? {} : parseUserReadStatuses(thread.metadata.userReadStatuses)
  switch (status) {
    case 'read':
      if (userReadStatuses[userId] === true) {
        return
      }
      userReadStatuses[userId] = true
      break
    case 'unread':
      if (userReadStatuses[userId] == null || userReadStatuses[userId] === false) {
        return
      }
      delete userReadStatuses[userId]
      break
    default:
      assertNever(status)
  }
  editThreadMetadata({
    threadId: thread.id,
    metadata: {
      userReadStatuses: printUserReadStatuses(userReadStatuses),
    },
  })
}
