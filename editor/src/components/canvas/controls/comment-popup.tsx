import type { CommentData } from '@liveblocks/client'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { Button, UtopiaStyles, useColorTheme } from '../../../uuiui'
import React from 'react'
import { useCreateThread, useStorage } from '../../../../liveblocks.config'
import '../../../../resources/editor/css/liveblocks-comments.css'
import {
  useCanvasCommentThreadAndLocation,
  useResolveThread,
} from '../../../core/commenting/comment-hooks'
import { useRemixPresence } from '../../../core/shared/multiplayer-hooks'
import { CommentWrapper, MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { switchEditorMode } from '../../editor/actions/action-creators'
import type { CommentId } from '../../editor/editor-modes'
import {
  EditorModes,
  existingComment,
  isCommentMode,
  isNewComment,
} from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { assertNever } from '../../../core/shared/utils'
import { when } from '../../../utils/react-conditionals'

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

  const commentsCount = React.useMemo(
    () => thread?.comments.filter((c) => c.deletedAt == null).length ?? 0,
    [thread],
  )

  const createThread = useCreateThread()

  const remixPresence = useRemixPresence()

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
                remixLocationRoute: remixPresence?.locationRoute ?? undefined,
              },
            })
          case 'scene':
            return createThread({
              body,
              metadata: {
                resolved: false,
                type: 'canvas',
                x: comment.location.offset.x,
                y: comment.location.offset.y,
                sceneId: comment.location.sceneId,
                remixLocationRoute: remixPresence?.locationRoute ?? undefined,
              },
            })
          default:
            assertNever(comment.location)
        }
      })()
      dispatch([
        switchEditorMode(EditorModes.commentMode(existingComment(newThread.id), 'not-dragging')),
      ])
    },
    [createThread, comment, remixPresence, dispatch],
  )

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
            const user = collabs[c.userId]
            return (
              <CommentWrapper
                key={c.id}
                user={user}
                comment={c}
                onCommentDelete={onCommentDelete}
              />
            )
          })}
          <Composer autoFocus threadId={thread.id} />
        </>
      )}
    </div>
  )
})
CommentThread.displayName = 'CommentThread'
