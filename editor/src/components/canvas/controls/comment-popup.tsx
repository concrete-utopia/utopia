import type { CommentData } from '@liveblocks/client'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import React from 'react'
import { useCreateThread } from '../../../../liveblocks.config'
import '../../../../resources/editor/css/liveblocks-comments.css'
import { useCanvasCommentThread } from '../../../core/commenting/comment-hooks'
import { useRemixPresence } from '../../../core/shared/multiplayer-hooks'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { UtopiaTheme } from '../../../uuiui'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { EditorModes, isCommentMode } from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const CommentPopup = React.memo(() => {
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'CommentPopup mode',
  )

  if (!isCommentMode(mode) || mode.location == null) {
    return null
  }
  const { location } = mode

  return (
    <CanvasOffsetWrapper>
      <div
        style={{
          position: 'absolute',
          top: location.y,
          left: location.x + 30,
          cursor: 'text',
          minWidth: 250,
          boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        }}
        onKeyDown={stopPropagation}
        onKeyUp={stopPropagation}
        onMouseUp={stopPropagation}
      >
        <MultiplayerWrapper
          errorFallback={<div>Can not load comments</div>}
          suspenseFallback={<div>Loading…</div>}
        >
          <CommentThread x={location.x} y={location.y} />
        </MultiplayerWrapper>
      </div>
    </CanvasOffsetWrapper>
  )
})
CommentPopup.displayName = 'CommentPopup'

interface CommentThreadProps {
  x: number
  y: number
}

const CommentThread = React.memo(({ x, y }: CommentThreadProps) => {
  const dispatch = useDispatch()
  const thread = useCanvasCommentThread(x, y)
  const commentsCount = React.useMemo(
    () => thread?.comments.filter((c) => c.deletedAt == null).length ?? 0,
    [thread],
  )

  const createThread = useCreateThread()

  const remixPresence = useRemixPresence()

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      // Create a new thread
      createThread({
        body,
        metadata: {
          type: 'canvas',
          x: x,
          y: y,
          remixLocationRoute: remixPresence?.locationRoute ?? undefined,
        },
      })
    },
    [createThread, x, y, remixPresence],
  )

  const onCommentDelete = React.useCallback(
    (_deleted: CommentData) => {
      if (commentsCount - 1 <= 0) {
        dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
      }
    },
    [commentsCount, dispatch],
  )

  if (thread == null) {
    return <Composer autoFocus onComposerSubmit={onCreateThread} />
  }

  return (
    <div>
      {thread.comments.map((comment) => (
        <Comment key={comment.id} comment={comment} onCommentDelete={onCommentDelete} />
      ))}
      <Composer autoFocus threadId={thread.id} />
    </div>
  )
})
CommentThread.displayName = 'CommentThread'
