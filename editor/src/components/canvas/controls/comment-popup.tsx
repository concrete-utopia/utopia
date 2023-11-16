import '@liveblocks/react-comments/styles.css'
import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { isCommentMode } from '../../editor/editor-modes'
import { ClientSideSuspense } from '@liveblocks/react'
import { useCreateThread } from '../../../../liveblocks.config'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { UtopiaTheme } from '../../../uuiui'
import { ErrorBoundary } from '../../../utils/react-error-boundary'
import {
  useCanvasCommentThread,
  useMyMultiplayerColorIndex,
} from '../../../core/commenting/comment-hooks'
import { isLoggedIn } from '../../editor/action-types'

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
        <ErrorBoundary fallback={<div>Can not load comments</div>}>
          <ClientSideSuspense fallback={<div>Loading…</div>}>
            {() => <CommentThread x={location.x} y={location.y} />}
          </ClientSideSuspense>
        </ErrorBoundary>
      </div>
    </CanvasOffsetWrapper>
  )
})

interface CommentThreadProps {
  x: number
  y: number
}

function CommentThread({ x, y }: CommentThreadProps) {
  const thread = useCanvasCommentThread(x, y)

  const createThread = useCreateThread()

  const colorIndex = useMyMultiplayerColorIndex() ?? -1

  // TODO: Unify getting name in different multiplayer components
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'CommentThread loginState',
  )

  const name = isLoggedIn(loginState) ? loginState.user.name : null

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      // Create a new thread
      createThread({
        body,
        metadata: { type: 'canvas', x: x, y: y, name: name ?? 'Anonymous', colorIndex: colorIndex },
      })
    },
    [createThread, x, y, name, colorIndex],
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
