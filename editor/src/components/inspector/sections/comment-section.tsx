import '@liveblocks/react-comments/styles.css'
import React from 'react'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { FlexRow, InspectorSubsectionHeader } from '../../../uuiui'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagComment,
} from '../../../core/shared/comment-flags'
import { RoomProvider, useCreateThread, useThreads } from '../../../../liveblocks.config'
import { ClientSideSuspense, createRoomContext } from '@liveblocks/react'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../common/inspector-utils'
import { useDispatch } from '../../editor/store/dispatch-context'
import { setCommentId } from '../../editor/actions/action-creators'
import { findMaybeConditionalExpression } from '../../../core/model/conditionals'

export const CommentSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const element = useEditorState(
    Substores.metadata,
    (store) => {
      if (paths.length === 0) {
        return null
      }
      return findMaybeConditionalExpression(paths[0], store.editor.jsxMetadata)
    },
    'CommentSection element',
  )

  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'CommentSection projectId',
  )

  if (projectId == null) {
    return null
  }

  if (element == null) {
    return null
  }

  if (paths.length !== 1) {
    return null
  }

  const flag = findUtopiaCommentFlag(element.comments, 'comment')
  const threadId = isUtopiaCommentFlagComment(flag) ? flag.value : null

  return (
    <div onKeyDown={stopPropagation} onKeyUp={stopPropagation}>
      <RoomProvider id={projectId} initialPresence={{}}>
        <InspectorSubsectionHeader>
          <FlexRow
            style={{
              flexGrow: 1,
              gap: 8,
              height: 42,
            }}
          >
            <span>Comments</span>
          </FlexRow>
        </InspectorSubsectionHeader>
        <ClientSideSuspense fallback={<div>Loading…</div>}>
          {() => <Room id={threadId} path={paths[0]} />}
        </ClientSideSuspense>
      </RoomProvider>
    </div>
  )
})
CommentSection.displayName = 'CommentSection'

interface RoomProps {
  id: string | null
  path: ElementPath
}

function Room(props: RoomProps) {
  const { threads } = useThreads()

  const createThread = useCreateThread()

  const userName = useEditorState(
    Substores.userState,
    (store) => {
      if (store.userState.loginState.type !== 'LOGGED_IN') {
        return null
      }
      return store.userState.loginState.user.name
    },
    'Room userName',
  )

  const threadId = props.id
  const thread = threads.find((t) => t.id === threadId)

  const dispatch = useDispatch()

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      // Create a new thread
      const newThread = createThread({
        body,
        metadata: {},
      })

      dispatch([setCommentId(props.path, newThread.id)])
    },
    [createThread, dispatch, props.path],
  )

  if (threadId == null || thread == null) {
    return <Composer onComposerSubmit={onCreateThread} />
  }

  return (
    <div>
      {thread.comments.map((comment) => (
        <Comment key={comment.id} comment={comment} />
      ))}
      <Composer threadId={threadId} />
    </div>
  )
}
