/** @jsxRuntime classic */
/** @jsx jsx */
import { css, jsx } from '@emotion/react'
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
import { Button, FlexRow, Icn, Tooltip, UtopiaStyles, useColorTheme } from '../../../uuiui'
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
import { motion, useAnimation } from 'framer-motion'
import { CSSCursor } from '../canvas-types'
import type { EditorDispatch } from '../../editor/action-types'

export const ComposerEditorClassName = 'lb-composer-editor'

export function getComposerTextbox(): HTMLDivElement | null {
  const editorsByClass = document.getElementsByClassName(ComposerEditorClassName)
  if (editorsByClass.length < 1) {
    return null
  }
  return editorsByClass[0] as HTMLDivElement
}

const PopupMaxWidth = 250
const PopupMaxHeight = 350

const ComposerStyle: CSSProperties = {
  maxWidth: PopupMaxWidth,
  wordWrap: 'break-word',
  whiteSpace: 'normal',
  zIndex: 10,
}

function switchToBasicCommentModeOnEscape(e: React.KeyboardEvent, dispatch: EditorDispatch) {
  if (e.key === 'Escape') {
    dispatch([switchEditorMode(EditorModes.commentMode(null, 'not-dragging'))])
  }
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

      const composerTextbox = getComposerTextbox()
      if (composerTextbox == null) {
        return null
      }

      scrollToBottom()

      return composerTextbox
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

  const onClickClose = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.commentMode(null, 'not-dragging'))])
  }, [dispatch])

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

  React.useEffect(() => {
    // when the thread id changes, re-check the scroll and set the inset shadow
    onScroll()
    scrollToBottom()
  }, [threadId, scrollToBottom])

  const onExistingCommentComposerKeyDown = React.useCallback(
    (e: React.KeyboardEvent) => switchToBasicCommentModeOnEscape(e, dispatch),
    [dispatch],
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
        <NewCommentPopup onComposerSubmit={onCreateThread} />
      ) : (
        <div>
          <FlexRow
            style={{
              background: colorTheme.bg1.value,
              justifyContent: 'flex-end',
              padding: 6,
              borderBottom: `1px solid ${colorTheme.bg3.value}`,
              gap: 6,
            }}
          >
            {when(
              readByMe === 'read',
              <Tooltip title='Mark As Unread' placement='top'>
                <Button onClick={onClickMarkAsUnread}>
                  <Icn category='semantic' type='unread' width={18} height={18} color='main' />
                </Button>
              </Tooltip>,
            )}
            <Tooltip title='Resolve' placement='top'>
              <Button onClick={onClickResolve}>
                <Icn
                  category='semantic'
                  type={thread?.metadata.resolved ? 'resolved' : 'resolve'}
                  width={18}
                  height={18}
                  color='main'
                />
              </Button>
            </Tooltip>
            <Button
              onClick={onClickClose}
              css={{
                '&:hover': {
                  opacity: 0.5,
                },
              }}
            >
              <Icn category='semantic' type='cross-large' width={16} height={16} color='main' />
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
            onKeyDown={onExistingCommentComposerKeyDown}
          />
        </div>
      )}
    </div>
  )
})
CommentThread.displayName = 'CommentThread'

type NewCommentPopupProps = {
  onComposerSubmit: (
    comment: ComposerSubmitComment,
    event: React.FormEvent<HTMLFormElement>,
  ) => void
}

const NewCommentPopup = React.memo((props: NewCommentPopupProps) => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const theme = useEditorState(
    Substores.userState,
    (store) => getCurrentTheme(store.userState),
    'NewCommentPopup theme',
  )

  const onNewCommentComposerKeyDown = React.useCallback(
    (e: React.KeyboardEvent) => switchToBasicCommentModeOnEscape(e, dispatch),
    [dispatch],
  )

  const newCommentComposerAnimation = useAnimation()

  const onClickOutsideNewComment = React.useCallback(
    (e: React.MouseEvent) => {
      e.preventDefault()
      e.stopPropagation()

      const composerTextbox = getComposerTextbox()
      if (composerTextbox != null) {
        function findPlaceholderChild(element: Element) {
          if (element == null) {
            return false
          }
          if (element.attributes.getNamedItem('data-placeholder') != null) {
            return true
          }
          if (element.children.length < 1) {
            return false
          }
          return findPlaceholderChild(element.children[0])
        }

        const isEmpty = composerTextbox.innerText.trim().length === 0
        const isPlaceholder = !isEmpty && findPlaceholderChild(composerTextbox.children[0])

        // if the contents of the new comment are empty...
        if (isEmpty || isPlaceholder) {
          // ...just close the popup
          dispatch([switchEditorMode(EditorModes.commentMode(null, 'not-dragging'))])
        } else {
          // ...otherwise, shake the popup and re-focus its text box
          const shakeDelta = 4 // px
          void newCommentComposerAnimation.start({
            x: [-shakeDelta, shakeDelta, -shakeDelta, shakeDelta, 0],
            borderColor: [
              colorTheme.error.cssValue,
              colorTheme.error.cssValue,
              colorTheme.error.cssValue,
              colorTheme.error.cssValue,
              '#00000000', // transparent, animatable
            ],
            transition: { duration: 0.2 },
          })
        }

        composerTextbox.focus()
      }
    },
    [newCommentComposerAnimation, colorTheme, dispatch],
  )

  return (
    <div>
      <div
        style={{
          background: 'transparent',
          position: 'fixed',
          top: 0,
          left: 0,
          bottom: 0,
          right: 0,
          cursor: CSSCursor.Comment,
        }}
        onClick={onClickOutsideNewComment}
      />
      <motion.div animate={newCommentComposerAnimation} style={{ border: '1px solid transparent' }}>
        <Composer
          data-theme={theme}
          autoFocus
          onComposerSubmit={props.onComposerSubmit}
          style={ComposerStyle}
          onKeyDown={onNewCommentComposerKeyDown}
        />
      </motion.div>
    </div>
  )
})
NewCommentPopup.displayName = 'NewCommentPopup'

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
