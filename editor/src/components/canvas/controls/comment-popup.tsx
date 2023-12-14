import type { CommentData } from '@liveblocks/client'
import type { ComposerSubmitComment } from '@liveblocks/react-comments'
import { Composer } from '@liveblocks/react-comments'
import { useAtom } from 'jotai'
import type { CSSProperties } from 'react'
import React, { useRef } from 'react'
import { useCreateThread, useStorage } from '../../../../liveblocks.config'
import '../../../../resources/editor/css/liveblocks/react-comments/styles.css'
import '../../../../resources/editor/css/liveblocks/react-comments/dark/attributes.css'
import {
  getCollaboratorById,
  useCanvasCommentThreadAndLocation,
  useCreateNewThreadReadStatus,
  useDeleteThreadReadStatus,
  useMyThreadReadStatus,
  useResolveThread,
  useScenes,
  useSetThreadReadStatus,
  useSetThreadReadStatusOnMount,
} from '../../../core/commenting/comment-hooks'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import { create } from '../../../core/shared/property-path'
import { assertNever } from '../../../core/shared/utils'
import { CommentWrapper, MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { when } from '../../../utils/react-conditionals'
import { Button, FlexRow, Icn, UtopiaStyles, useColorTheme } from '../../../uuiui'
import {
  setProp_UNSAFE,
  setRightMenuTab,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import type { CommentId } from '../../editor/editor-modes'
import {
  EditorModes,
  existingComment,
  isCommentMode,
  isNewComment,
} from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { RightMenuTab, getCurrentTheme } from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { RemixNavigationAtom } from '../remix/utopia-remix-root-component'
import { getIdOfScene } from './comment-mode/comment-mode-hooks'
import { Tooltip } from 'antd'

const ComposerEditorClassName = 'lb-composer-editor'

const PopupMaxWidth = 250
const PopupMaxHeight = 350

const ComposerStyle: CSSProperties = {
  maxWidth: PopupMaxWidth,
  wordWrap: 'break-word',
  whiteSpace: 'normal',
}

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

  const composerRef = useRef<HTMLFormElement | null>(null)
  const listRef = React.useRef<HTMLDivElement | null>(null)

  const [showShadowBottom, setShowShadowBottom] = React.useState(false)
  const [showShadowTop, setShowShadowTop] = React.useState(false)

  const { location, thread } = useCanvasCommentThreadAndLocation(comment)
  const threadId = thread?.id ?? null

  useSetThreadReadStatusOnMount(thread)
  const setThreadReadStatus = useSetThreadReadStatus()
  const createNewThreadReadStatus = useCreateNewThreadReadStatus()
  const deleteThreadReadStatus = useDeleteThreadReadStatus()

  const readByMe = useMyThreadReadStatus(thread)

  const commentsCount = React.useMemo(
    () => thread?.comments.filter((c) => c.deletedAt == null).length ?? 0,
    [thread],
  )

  const createThread = useCreateThread()

  const scenes = useScenes()
  const [remixSceneRoutes] = useAtom(RemixNavigationAtom)

  const scrollToBottom = React.useCallback(() => {
    if (listRef.current != null) {
      listRef.current.scrollTo({
        top: listRef.current.scrollHeight,
      })
    }
  }, [])

  const theme = useEditorState(
    Substores.userState,
    (store) => getCurrentTheme(store.userState),
    'CommentThread theme',
  )

  const onCreateThread = React.useCallback(
    ({ body }: ComposerSubmitComment, event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault()

      if (!isNewComment(comment)) {
        return
      }

      // Create a new thread
      const [newThread, auxiliaryActions] = (() => {
        switch (comment.location.type) {
          case 'canvas':
            const newThreadOnCanvas = createThread({
              body,
              metadata: {
                resolved: false,
                type: 'canvas',
                x: comment.location.position.x,
                y: comment.location.position.y,
              },
            })
            return [newThreadOnCanvas, []]
          case 'scene':
            const sceneId = comment.location.sceneId
            const scene = scenes.find(
              (s) => getIdOfScene(s) === sceneId || EP.toUid(s.elementPath) === sceneId,
            )
            const remixRoute =
              scene != null ? remixSceneRoutes[EP.toString(scene?.elementPath)] : undefined

            const addSceneIdPropAction =
              scene == null
                ? []
                : [
                    setProp_UNSAFE(
                      scene.elementPath,
                      create('id'),
                      jsExpressionValue(sceneId, emptyComments),
                    ),
                  ]

            const newThreadOnScene = createThread({
              body,
              metadata: {
                resolved: false,
                type: 'canvas',
                x: comment.location.offset.x,
                y: comment.location.offset.y,
                sceneId: sceneId,
                remixLocationRoute: remixRoute != null ? remixRoute.location.pathname : undefined,
              },
            })
            return [newThreadOnScene, addSceneIdPropAction]
          default:
            assertNever(comment.location)
        }
      })()
      createNewThreadReadStatus(newThread.id, 'read')
      dispatch([
        ...auxiliaryActions,
        switchEditorMode(EditorModes.commentMode(existingComment(newThread.id), 'not-dragging')),
        setRightMenuTab(RightMenuTab.Comments),
      ])
    },
    [createThread, comment, dispatch, remixSceneRoutes, scenes, createNewThreadReadStatus],
  )

  const onSubmitComment = React.useCallback(() => {
    if (threadId != null) {
      createNewThreadReadStatus(threadId, 'read')
    }
    function getLiveblocksEditorElement(): HTMLDivElement | null {
      if (composerRef.current == null) {
        return null
      }
      const editorsByClass = composerRef.current.getElementsByClassName(ComposerEditorClassName)
      if (editorsByClass.length < 1) {
        return null
      }

      scrollToBottom()

      return editorsByClass[0] as HTMLDivElement
    }
    setTimeout(() => {
      getLiveblocksEditorElement()?.focus()
    }, 0)
  }, [threadId, createNewThreadReadStatus, scrollToBottom])

  const onCommentDelete = React.useCallback(
    (_deleted: CommentData) => {
      if (commentsCount - 1 <= 0) {
        dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
        if (threadId != null) {
          deleteThreadReadStatus(threadId)
        }
      }
    },
    [commentsCount, dispatch, threadId, deleteThreadReadStatus],
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

  const onClickMarkAsUnread = React.useCallback(() => {
    if (thread?.id == null) {
      return
    }
    setThreadReadStatus(thread.id, 'unread')
  }, [thread?.id, setThreadReadStatus])

  const collabs = useStorage((storage) => storage.collaborators)

  const onScroll = () => {
    const element = listRef.current
    if (element == null) {
      return
    }
    const tolerance = 20 // px

    const atBottom = element.scrollHeight - element.scrollTop <= PopupMaxHeight + tolerance
    const isOverflowing = element.scrollHeight > PopupMaxHeight
    setShowShadowBottom(!atBottom && isOverflowing)

    const atTop = element.scrollTop > tolerance
    setShowShadowTop(atTop && isOverflowing)
  }

  const onClickClose = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.commentMode(null, 'not-dragging'))])
  }, [dispatch])

  React.useEffect(() => {
    // when the thread id changes, re-check the scroll and set the inset shadow
    onScroll()
    scrollToBottom()
  }, [threadId, scrollToBottom])

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
        borderRadius: 4,
        overflow: 'hidden',
      }}
      onKeyDown={stopPropagation}
      onKeyUp={stopPropagation}
      onMouseUp={stopPropagation}
    >
      {thread == null ? (
        <Composer
          data-theme={theme}
          autoFocus
          onComposerSubmit={onCreateThread}
          style={ComposerStyle}
        />
      ) : (
        <>
          <FlexRow
            style={{
              background: colorTheme.bg1.value,
              justifyContent: 'flex-end',
              padding: 6,
              borderBottom: `1px solid ${colorTheme.bg3.value}`,
              gap: 4,
            }}
          >
            {when(
              readByMe === 'read',
              <Tooltip title='Mark As Unread' placement='top'>
                <Button onClick={onClickMarkAsUnread}>
                  <Icn category='semantic' type='unread' width={16} height={16} color='main' />
                </Button>
              </Tooltip>,
            )}
            <Tooltip title='Resolve' placement='top'>
              <Button onClick={onClickResolve}>
                <Icn
                  category='semantic'
                  type={thread?.metadata.resolved ? 'resolved' : 'resolve'}
                  width={16}
                  height={16}
                  color='main'
                />
              </Button>
            </Tooltip>
            <Button onClick={onClickClose}>
              <Icn category='semantic' type='cross-medium' width={16} height={16} color='main' />
            </Button>
          </FlexRow>
          <div style={{ position: 'relative' }}>
            <div
              style={{
                maxHeight: PopupMaxHeight,
                overflowY: 'scroll',
                maxWidth: PopupMaxWidth,
                wordWrap: 'break-word',
                whiteSpace: 'normal',
              }}
              ref={listRef}
              onScroll={onScroll}
            >
              {thread.comments.map((c) => {
                const user = getCollaboratorById(collabs, c.userId)
                return (
                  <CommentWrapper
                    key={c.id}
                    data-theme={theme}
                    user={user}
                    comment={c}
                    onCommentDelete={onCommentDelete}
                  />
                )
              })}
            </div>
            <ListShadow position='top' enabled={showShadowTop} />
            <ListShadow position='bottom' enabled={showShadowBottom} />
            <HeaderComment
              enabled={thread.comments.length > 0 && showShadowTop}
              comment={thread.comments[0]}
            />
          </div>
          <Composer
            ref={composerRef}
            data-theme={theme}
            autoFocus
            threadId={thread.id}
            onComposerSubmit={onSubmitComment}
            style={ComposerStyle}
          />
        </>
      )}
    </div>
  )
})
CommentThread.displayName = 'CommentThread'

const ListShadow = React.memo(
  ({ enabled, position }: { enabled: boolean; position: 'top' | 'bottom' }) => {
    const colorTheme = useColorTheme()
    return (
      <div
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          zIndex: 1,
          boxShadow: `inset 0 ${position === 'top' ? '20px' : '-20px'} 15px -10px ${
            colorTheme.shadow30.value
          }`,
          pointerEvents: 'none',
          opacity: enabled ? 1 : 0,
          transition: 'opacity 150ms linear',
        }}
      />
    )
  },
)
ListShadow.displayName = 'ListShadow'

const HeaderComment = React.memo(
  ({ comment, enabled }: { comment: CommentData; enabled: boolean }) => {
    const colorTheme = useColorTheme()
    const collabs = useStorage((storage) => storage.collaborators)
    const user = getCollaboratorById(collabs, comment.userId)
    return (
      <div
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          right: 0,
          backgroundColor: 'white',
          zIndex: 1,
          boxShadow: UtopiaStyles.shadowStyles.highest.boxShadow,
          // border: `1px solid ${colorTheme.primary50.value}`,
          opacity: enabled ? 1 : 0,
          transition: 'all 100ms linear',
          minHeight: 67,
          transform: 'scale(1.01)',
        }}
      >
        <CommentWrapper user={user} comment={comment} />
      </div>
    )
  },
)
HeaderComment.displayName = 'HeaderComment'
