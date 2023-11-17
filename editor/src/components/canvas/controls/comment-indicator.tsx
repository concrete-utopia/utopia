/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { EditorModes } from '../../editor/editor-modes'
import { ClientSideSuspense } from '@liveblocks/react'
import { useThreads } from '../../../../liveblocks.config'
import { useDispatch } from '../../editor/store/dispatch-context'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { canvasPoint } from '../../../core/shared/math-utils'
import { UtopiaTheme } from '../../../uuiui'
import { ErrorBoundary } from '../../../utils/react-error-boundary'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../../../core/shared/multiplayer'

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
      <ErrorBoundary fallback={null}>
        <ClientSideSuspense fallback={null}>{() => <CommentIndicatorInner />}</ClientSideSuspense>
      </ErrorBoundary>
    </CanvasOffsetWrapper>
  )
})

function CommentIndicatorInner() {
  const { threads } = useThreads()
  const dispatch = useDispatch()

  return (
    <React.Fragment>
      {threads.map((thread) => {
        const point = canvasPoint(thread.metadata)
        // TODO: unify initial handling for multiplayer
        const initials = multiplayerInitialsFromName(normalizeMultiplayerName(thread.metadata.name))
        const color = multiplayerColorFromIndex(thread.metadata.colorIndex)
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
