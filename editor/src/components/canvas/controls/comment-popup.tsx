import type { CommentData } from '@liveblocks/client'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Composer } from '@liveblocks/react-comments'
import { useAtom } from 'jotai'
import React from 'react'
import {
  useCreateThread,
  useEditThreadMetadata,
  useSelf,
  useStorage,
} from '../../../../liveblocks.config'
import '../../../../resources/editor/css/liveblocks-comments.css'
import {
  getCollaboratorById,
  useCanvasCommentThreadAndLocation,
  useCreateNewCommentThreadReadStatus,
  useMyCommentThreadReadStatus,
  useResolveThread,
  useScenesWithId,
  useSetCommentThreadReadStatus,
  useSetCommentThreadReadStatusOnMount,
} from '../../../core/commenting/comment-hooks'
import * as EP from '../../../core/shared/element-path'
import { assertNever } from '../../../core/shared/utils'
import { CommentWrapper, MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { when } from '../../../utils/react-conditionals'
import { Button, UtopiaStyles, useColorTheme } from '../../../uuiui'
import { setRightMenuTab, switchEditorMode } from '../../editor/actions/action-creators'
import type { CommentId } from '../../editor/editor-modes'
import {
  EditorModes,
  existingComment,
  isCommentMode,
  isNewComment,
} from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { RightMenuTab } from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { RemixNavigationAtom } from '../remix/utopia-remix-root-component'
import { getIdOfScene } from './comment-mode/comment-mode-hooks'

export const CommentPopup = React.memo(() => {
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'CommentPopup mode',
  )

  if (!isCommentMode(mode) || mode.comment == null) {
    return null
  }

  return (
    <MultiplayerWrapper
      errorFallback={<div>Can not load comments</div>}
      suspenseFallback={<div>Loading…</div>}
    >
      <CommentThread comment={mode.comment} />
    </MultiplayerWrapper>
  )
})
CommentPopup.displayName = 'CommentPopup'

interface CommentThreadProps {
  comment: CommentId
}

const CommentThread = React.memo(({ comment }: CommentThreadProps) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const { location, thread } = useCanvasCommentThreadAndLocation(comment)

  useSetCommentThreadReadStatusOnMount(thread)
  const setCommentThreadReadStatus = useSetCommentThreadReadStatus()
  const createNewCommentThreadReadStatus = useCreateNewCommentThreadReadStatus()

  const readByMe = useMyCommentThreadReadStatus(thread)

  const commentsCount = React.useMemo(
    () => thread?.comments.filter((c) => c.deletedAt == null).length ?? 0,
    [thread],
  )

  const createThread = useCreateThread()

  const scenes = useScenesWithId()
  const [remixSceneRoutes] = useAtom(RemixNavigationAtom)

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      if (!isNewComment(comment)) {
        return
      }

      // Create a new thread
      const newThread = (() => {
        switch (comment.location.type) {
          case 'canvas':
            return createThread({
              body,
              metadata: {
                resolved: false,
                type: 'canvas',
                x: comment.location.position.x,
                y: comment.location.position.y,
              },
            })
          case 'scene':
            const sceneId = comment.location.sceneId
            const scene = scenes.find((s) => getIdOfScene(s) === sceneId)
            const remixRoute =
              scene != null ? remixSceneRoutes[EP.toString(scene?.elementPath)] : undefined

            return createThread({
              body,
              metadata: {
                resolved: false,
                type: 'canvas',
                x: comment.location.offset.x,
                y: comment.location.offset.y,
                sceneId: sceneId,
                remixLocationRoute: remixRoute != null ? remixRoute.location.pathname : undefined,
              },
            })
          default:
            assertNever(comment.location)
        }
      })()
      createNewCommentThreadReadStatus(newThread.id, 'read')
      dispatch([
        switchEditorMode(EditorModes.commentMode(existingComment(newThread.id), 'not-dragging')),
        setRightMenuTab(RightMenuTab.Comments),
      ])
    },
    [createThread, comment, dispatch, remixSceneRoutes, scenes, createNewCommentThreadReadStatus],
  )

  const onSubmitComment = React.useCallback(() => {
    if (thread?.id != null) {
      createNewCommentThreadReadStatus(thread.id, 'read')
    }
  }, [thread?.id, createNewCommentThreadReadStatus])

  const onCommentDelete = React.useCallback(
    (_deleted: CommentData) => {
      if (commentsCount - 1 <= 0) {
        dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
      }
    },
    [commentsCount, dispatch],
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'CommentPopup canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'CommentPopup canvasScale',
  )

  const resolveThread = useResolveThread()

  const onClickResolve = React.useCallback(() => {
    if (thread == null) {
      return
    }
    resolveThread(thread)
  }, [thread, resolveThread])

  const onClickMarkAsUnread = React.useCallback(() => {
    if (thread?.id == null) {
      return
    }
    setCommentThreadReadStatus(thread.id, 'unread')
  }, [thread?.id, setCommentThreadReadStatus])

  const collabs = useStorage((storage) => storage.collaborators)

  if (location == null) {
    return null
  }

  const point = canvasPointToWindowPoint(location, canvasScale, canvasOffset)

  return (
    <div
      style={{
        position: 'fixed',
        top: point.y,
        left: point.x + 30,
        cursor: 'text',
        minWidth: 250,
        boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
        background: colorTheme.bg0.value,
      }}
      onKeyDown={stopPropagation}
      onKeyUp={stopPropagation}
      onMouseUp={stopPropagation}
    >
      {when(
        thread != null,
        <div
          style={{
            position: 'relative',
          }}
        >
          <div
            style={{
              position: 'absolute',
              left: 0,
              right: 0,
              top: -40,
              zIndex: 1,
              display: 'flex',
              alignItems: 'flex-end',
              justifyContent: 'flex-end',
              height: 40,
            }}
          >
            {when(
              readByMe === 'read',
              <Button
                highlight
                spotlight
                style={{ padding: '0 6px' }}
                onClick={onClickMarkAsUnread}
              >
                Mark as unread
              </Button>,
            )}
            <div style={{ width: 8 }} />
            <Button highlight spotlight style={{ padding: '0 6px' }} onClick={onClickResolve}>
              {thread?.metadata.resolved ? 'Unresolve' : 'Resolve'}
            </Button>
          </div>
        </div>,
      )}
      {thread == null ? (
        <Composer autoFocus onComposerSubmit={onCreateThread} />
      ) : (
        <>
          {thread.comments.map((c) => {
            const user = getCollaboratorById(collabs, c.userId)
            return (
              <CommentWrapper
                key={c.id}
                user={user}
                comment={c}
                onCommentDelete={onCommentDelete}
              />
            )
          })}
          <Composer autoFocus threadId={thread.id} onComposerSubmit={onSubmitComment} />
        </>
      )}
    </div>
  )
})
CommentThread.displayName = 'CommentThread'
