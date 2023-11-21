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
import { UtopiaTheme } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../../../core/shared/multiplayer'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'

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

function CommentIndicatorInner() {
  const { threads } = useThreads()
  const dispatch = useDispatch()
  const collabs = useStorage((storage) => storage.collaborators)

  return (
    <React.Fragment>
      {threads.map((thread) => {
        const point = canvasPoint(thread.metadata)
        const { initials, color } = (() => {
          const firstComment = thread.comments[0]
          if (firstComment == null) {
            return { initials: 'AN', color: multiplayerColorFromIndex(null) }
          }
          const author = collabs[firstComment.userId]
          if (author == null) {
            return { initials: 'AN', color: multiplayerColorFromIndex(null) }
          }
          return {
            initials: multiplayerInitialsFromName(normalizeMultiplayerName(author.name)),
            color: multiplayerColorFromIndex(author.colorIndex),
          }
        })()

        return (
          <div
            key={thread.id}
            style={{
              position: 'absolute',
              top: point.y,
              left: point.x,
              width: 20,
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
                  boxShadow: UtopiaTheme.panelStyles.shadows.medium,
                }}
              >
                {initials}
              </div>
            </div>
          </div>
        )
      })}
    </React.Fragment>
  )
}
