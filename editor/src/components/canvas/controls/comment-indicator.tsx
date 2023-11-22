/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { EditorModes } from '../../editor/editor-modes'
import { useStorage, useThreads } from '../../../../liveblocks.config'
import { useDispatch } from '../../editor/store/dispatch-context'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { canvasPoint } from '../../../core/shared/math-utils'
import { UtopiaStyles, UtopiaTheme } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../../../core/shared/multiplayer'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { AvatarPicture } from '../../user-bar'

export const CommentIndicator = React.memo(() => {
  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'CommentIndicator projectId',
  )

  if (projectId == null) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
        <CommentIndicatorInner />
      </MultiplayerWrapper>
    </CanvasOffsetWrapper>
  )
})
CommentIndicator.displayName = 'CommentIndicator'

const CommentIndicatorInner = React.memo(() => {
  const { threads } = useThreads()
  const dispatch = useDispatch()
  const collabs = useStorage((storage) => storage.collaborators)

  return (
    <React.Fragment>
      {threads.map((thread) => {
        const point = canvasPoint(thread.metadata)
        const { initials, color, avatar } = (() => {
          const firstComment = thread.comments[0]
          if (firstComment == null) {
            return { initials: 'AN', color: multiplayerColorFromIndex(null), avatar: null }
          }
          const author = collabs[firstComment.userId]
          if (author == null) {
            return { initials: 'AN', color: multiplayerColorFromIndex(null), avatar: null }
          }
          return {
            initials: multiplayerInitialsFromName(normalizeMultiplayerName(author.name)),
            color: multiplayerColorFromIndex(author.colorIndex),
            avatar: author.avatar,
          }
        })()

        return (
          <div
            key={thread.id}
            css={{
              position: 'absolute',
              top: point.y,
              left: point.x,
              width: 20,
              '&:hover': {
                transform: 'scale(1.15)',
                transitionDuration: '0.1s',
              },
            }}
            onClick={() => {
              dispatch([switchEditorMode(EditorModes.commentMode(point))])
            }}
          >
            <div
              css={{
                height: 24,
                width: 24,
                background: 'black',
                border: '1px solid ',
                borderRadius: '24px 24px 24px 0px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              <div
                style={{
                  height: 20,
                  width: 20,
                  borderRadius: 10,
                  background: color.background,
                  color: color.foreground,
                  fontSize: 9,
                  fontWeight: 'bold',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
                }}
              >
                <AvatarPicture url={avatar} initials={initials} />
              </div>
            </div>
          </div>
        )
      })}
    </React.Fragment>
  )
})
CommentIndicatorInner.displayName = 'CommentIndicatorInner'
