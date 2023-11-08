import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { isCommentMode } from '../../editor/editor-modes'
import { ClientSideSuspense } from '@liveblocks/react'
import { useCreateThread, useThreads } from '../../../../liveblocks.config'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../../inspector/common/inspector-utils'

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
        style={{ position: 'absolute', top: location.y, left: location.x }}
        onKeyDown={stopPropagation}
        onKeyUp={stopPropagation}
      >
        <ClientSideSuspense fallback={<div>Loading…</div>}>
          {() => <Room id={`${location.x}-${location.y}`} />}
        </ClientSideSuspense>
      </div>
    </CanvasOffsetWrapper>
  )
})

interface RoomProps {
  id: string | null
}

function Room(props: RoomProps) {
  const { threads } = useThreads()
  const createThread = useCreateThread()

  const threadId = props.id
  const thread = threads.find((t) => (t.metadata as any).location === threadId)

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      // Create a new thread
      createThread({
        body,
        metadata: {
          type: 'coord',
          location: threadId,
        },
      })
    },
    [createThread, threadId],
  )

  if (threadId == null || thread == null) {
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
