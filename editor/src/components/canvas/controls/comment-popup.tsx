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
          left: location.x,
          cursor: 'text',
          minWidth: 250,
          boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        }}
        onKeyDown={stopPropagation}
        onKeyUp={stopPropagation}
        onMouseUp={stopPropagation}
      >
        <ClientSideSuspense fallback={<div>Loading…</div>}>
          {() => <Room top={location.y} left={location.x} />}
        </ClientSideSuspense>
      </div>
    </CanvasOffsetWrapper>
  )
})

interface RoomProps {
  top: number
  left: number
}

function Room(props: RoomProps) {
  const { threads } = useThreads()
  const createThread = useCreateThread()

  const thread = threads.find(
    (t) => (t.metadata as any).top === props.top && (t.metadata as any).left === props.left,
  )

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      // Create a new thread
      createThread({
        body,
        metadata: {
          type: 'coord',
          top: props.top,
          left: props.left,
        },
      })
    },
    [createThread, props.left, props.top],
  )

  if (thread == null) {
    return <Composer onComposerSubmit={onCreateThread} />
  }

  return (
    <div>
      {thread.comments.map((comment) => (
        <Comment key={comment.id} comment={comment} />
      ))}
      <Composer threadId={thread.id} />
    </div>
  )
}
