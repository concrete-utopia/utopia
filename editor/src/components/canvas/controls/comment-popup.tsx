import '../../../../resources/editor/css/liveblocks-comments.css'
import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { isCommentMode } from '../../editor/editor-modes'
import { useCreateThread } from '../../../../liveblocks.config'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { UtopiaTheme } from '../../../uuiui'
import { useCanvasCommentThread } from '../../../core/commenting/comment-hooks'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { useRemixPresence } from '../../../core/shared/multiplayer-hooks'

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
  const thread = useCanvasCommentThread(x, y)

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
          remixLocationPath: remixPresence?.locationPath ?? undefined,
        },
      })
    },
    [createThread, x, y, remixPresence],
  )

  if (thread == null) {
    return <Composer autoFocus onComposerSubmit={onCreateThread} />
  }

  return (
    <div>
      {thread.comments.map((comment) => (
        <Comment key={comment.id} comment={comment} />
      ))}
      <Composer autoFocus threadId={thread.id} />
    </div>
  )
})
CommentThread.displayName = 'CommentThread'
