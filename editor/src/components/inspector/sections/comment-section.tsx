import '@liveblocks/react-comments/styles.css'
import React from 'react'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { FlexColumn, FlexRow, InspectorSubsectionHeader, Section } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagComment,
} from '../../../core/shared/comment-flags'
import { useCreateThread, useThreads } from '../../../../liveblocks.config'
import { ClientSideSuspense } from '@liveblocks/react'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Comment, Composer } from '@liveblocks/react-comments'
import { stopPropagation } from '../common/inspector-utils'
import { useDispatch } from '../../editor/store/dispatch-context'
import { setCommentId } from '../../editor/actions/action-creators'
import { maybeConditionalExpression } from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getJSXAttributesAtPath } from '../../../core/shared/jsx-attributes'
import * as PP from '../../../core/shared/property-path'
import {
  isJSXConditionalExpression,
  isJSXElement,
  isJSXMapExpression,
} from '../../../core/shared/element-template'
import { isLeft, isRight } from '../../../core/shared/either'

export const CommentSection = React.memo(() => {
  const paths = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'CommentSection paths',
  )

  const element = useEditorState(
    Substores.metadata,
    (store) => {
      if (paths.length === 0) {
        return null
      }
      return MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, paths[0])
    },
    'CommentSection element',
  )

  if (element == null) {
    return null
  }

  if (paths.length !== 1) {
    return null
  }

  const threadId = (() => {
    if (isLeft(element.element)) {
      return null
    }
    const jsxElem = element.element.value
    if (isJSXConditionalExpression(jsxElem) || isJSXMapExpression(jsxElem)) {
      const flag = findUtopiaCommentFlag(jsxElem.comments, 'comment')
      return isUtopiaCommentFlagComment(flag) ? flag.value : null
    }

    if (isJSXElement(jsxElem)) {
      const attrs = getJSXAttributesAtPath(jsxElem.props, PP.create('data-comment'))
      if (attrs.attribute.type === 'ATTRIBUTE_VALUE') {
        return attrs.attribute.value
      }
    }
    return null
  })()

  return (
    <FlexColumn
      id='leftPaneSettings'
      key='leftPaneSettings'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
        overflowY: 'scroll',
        alignSelf: 'stretch',
      }}
    >
      <Section>
        <div onKeyDown={stopPropagation} onKeyUp={stopPropagation}>
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
        </div>
      </Section>
    </FlexColumn>
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
        metadata: {
          type: 'element',
        },
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
