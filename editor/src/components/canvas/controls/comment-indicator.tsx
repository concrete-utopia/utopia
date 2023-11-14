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

export const CommentIndicator = React.memo(() => {
  return (
    <CanvasOffsetWrapper>
      <ClientSideSuspense fallback={<div>Loading…</div>}>{() => <Room />}</ClientSideSuspense>
    </CanvasOffsetWrapper>
  )
})

function Room() {
  const { threads } = useThreads()

  const dispatch = useDispatch()
  return (
    <React.Fragment>
      {threads.map((thread) => {
        const commentData = (() => {
          if ((thread.metadata as any)?.type === 'coord') {
            const { top, left } = thread.metadata
            return { point: canvasPoint({ x: left, y: top }), type: 'coord' }
          }
          return null
        })()
        if (commentData == null) {
          return null
        }
        const { point } = commentData
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
              if (commentData == null) {
                return
              }
              if (commentData.type === 'coord') {
                dispatch([switchEditorMode(EditorModes.commentMode(point))])
              }
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
                  background: '#ff00a7',
                  color: 'white',
                  fontSize: 9,
                  fontWeight: 'bold',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  boxShadow: UtopiaTheme.panelStyles.shadows.medium,
                }}
              >
                AN
              </div>
            </div>
          </div>
        )
      })}
    </React.Fragment>
  )
}
