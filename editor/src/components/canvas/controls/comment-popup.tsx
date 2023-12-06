import type { CommentData } from '@liveblocks/client'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { UtopiaStyles } from '../../../uuiui'
import React from 'react'
import { useCreateThread } from '../../../../liveblocks.config'
import '../../../../resources/editor/css/liveblocks-comments.css'
import { useCanvasCommentThreadAndLocation } from '../../../core/commenting/comment-hooks'
import { useRemixPresence } from '../../../core/shared/multiplayer-hooks'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
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
      const newThread = createThread({
        body,
        metadata: {
          type: 'canvas',
          x: comment.location.x,
          y: comment.location.y,
          remixLocationRoute: remixPresence?.locationRoute ?? undefined,
        },
      })
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
        maxHeight: 300,
        overflowY: 'scroll',
        display: 'flex',
        flexDirection: 'column-reverse',
      }}
      onKeyDown={stopPropagation}
      onKeyUp={stopPropagation}
      onMouseUp={stopPropagation}
    >
      {thread == null ? (
        <Composer autoFocus onComposerSubmit={onCreateThread} />
      ) : (
        <>
          {thread.comments.reverse().map((c) => (
            <Comment key={c.id} comment={c} onCommentDelete={onCommentDelete} />
          ))}

          <Composer autoFocus threadId={thread.id} />
        </>
      )}
    </div>
  )
})
CommentThread.displayName = 'CommentThread'
