import '@liveblocks/react-comments/styles.css'
import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { isCommentMode } from '../../editor/editor-modes'
import { ClientSideSuspense } from '@liveblocks/react'
import { useCreateThread, useThreads } from '../../../../liveblocks.config'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { UtopiaTheme } from '../../../uuiui'

export const CommentPopup = React.memo(() => {
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'CommentIndicator mode',
  )

  if (!isCommentMode(mode) || mode.location == null) {
    return null
  }
  const { location } = mode
  if (location == null) {
    return null
  }

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
        <ClientSideSuspense fallback={<div>Loading…</div>}>
          {() => <CommentThread x={location.x} y={location.y} />}
        </ClientSideSuspense>
      </div>
    </CanvasOffsetWrapper>
  )
})

interface CommentThreadProps {
  x: number
  y: number
}

function CommentThread({ x, y }: CommentThreadProps) {
  const { threads } = useThreads()
  const createThread = useCreateThread()

  const thread = threads.find((t) => t.metadata.x === x && t.metadata.y === y)

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      // Create a new thread
      createThread({
        body,
        metadata: {
          type: 'coord',
          x: x,
          y: y,
        },
      })
    },
    [createThread, x, y],
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
}
