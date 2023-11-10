/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx, css } from '@emotion/react'
import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { EditorModes } from '../../editor/editor-modes'
import { ClientSideSuspense } from '@liveblocks/react'
import { useThreads } from '../../../../liveblocks.config'
import { useDispatch } from '../../editor/store/dispatch-context'
import { selectComponents, switchEditorMode } from '../../editor/actions/action-creators'
import { canvasPoint, isNotNullFiniteRectangle } from '../../../core/shared/math-utils'
import { UtopiaTheme } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isLeft } from '../../../core/shared/either'
import {
  isJSXConditionalExpression,
  isJSXElement,
  isJSXMapExpression,
} from '../../../core/shared/element-template'
import type { ElementInstanceMetadata } from '../../../core/shared/element-template'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagComment,
} from '../../../core/shared/comment-flags'
import { getJSXAttributesAtPath } from '../../../core/shared/jsx-attributes'
import * as PP from '../../../core/shared/property-path'

export const CommentIndicator = React.memo(() => {
  return (
    <CanvasOffsetWrapper>
      <ClientSideSuspense fallback={<div>Loading…</div>}>{() => <Room />}</ClientSideSuspense>
    </CanvasOffsetWrapper>
  )
})

function Room() {
  const { threads } = useThreads()

  const elementsWithComment = useEditorState(
    Substores.fullStore,
    (store) => {
      const elements = Object.values(store.editor.jsxMetadata)
      let result: Array<{ commentId: string; element: ElementInstanceMetadata }> = []
      elements.forEach((element) => {
        if (isLeft(element.element)) {
          return
        }
        const jsxElement = element.element.value
        if (isJSXConditionalExpression(jsxElement) || isJSXMapExpression(jsxElement)) {
          const flag = findUtopiaCommentFlag(jsxElement.comments, 'comment')
          if (isUtopiaCommentFlagComment(flag)) {
            result.push({ commentId: flag.value, element: element })
          }
        }
        if (isJSXElement(jsxElement)) {
          const attrs = getJSXAttributesAtPath(jsxElement.props, PP.create('data-comment'))
          if (attrs.attribute.type === 'ATTRIBUTE_VALUE') {
            result.push({ commentId: attrs.attribute.value, element: element })
          }
        }
      })
      return result
    },
    'CommentSection elementsWithComment',
  )

  const dispatch = useDispatch()
  return (
    <React.Fragment>
      {threads.map((thread) => {
        const commentData = (() => {
          if ((thread.metadata as any)?.type === 'coord') {
            const top = (thread.metadata as any).top
            const left = (thread.metadata as any).left
            return { point: canvasPoint({ x: left, y: top }), type: 'coord' }
          }
          const element = elementsWithComment.find((e) => e.commentId === thread.id)?.element
          const globalFrame = element?.globalFrame ?? null
          if (!isNotNullFiniteRectangle(globalFrame)) {
            return null
          }
          return {
            point: canvasPoint({
              x: globalFrame.x + globalFrame.width - 10,
              y: globalFrame.y - 10,
            }),
            type: 'element',
          }
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
              if (commentData.type == 'coord') {
                dispatch([switchEditorMode(EditorModes.commentMode(point))])
              }
              if (commentData.type == 'element') {
                const element = elementsWithComment.find((e) => e.commentId === thread.id)
                if (element != null) {
                  dispatch([selectComponents([element?.element.elementPath], false)])
                }
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

const CommentIcon = () => (
  <svg
    id='Layer_1'
    data-name='Layer 1'
    xmlns='http://www.w3.org/2000/svg'
    viewBox='0 0 121.86 122.88'
  >
    <title>comment</title>
    <path d='M30.28,110.09,49.37,91.78A3.84,3.84,0,0,1,52,90.72h60a2.15,2.15,0,0,0,2.16-2.16V9.82a2.16,2.16,0,0,0-.64-1.52A2.19,2.19,0,0,0,112,7.66H9.82A2.24,2.24,0,0,0,7.65,9.82V88.55a2.19,2.19,0,0,0,2.17,2.16H26.46a3.83,3.83,0,0,1,3.82,3.83v15.55ZM28.45,63.56a3.83,3.83,0,1,1,0-7.66h53a3.83,3.83,0,0,1,0,7.66Zm0-24.86a3.83,3.83,0,1,1,0-7.65h65a3.83,3.83,0,0,1,0,7.65ZM53.54,98.36,29.27,121.64a3.82,3.82,0,0,1-6.64-2.59V98.36H9.82A9.87,9.87,0,0,1,0,88.55V9.82A9.9,9.9,0,0,1,9.82,0H112a9.87,9.87,0,0,1,9.82,9.82V88.55A9.85,9.85,0,0,1,112,98.36Z' />
  </svg>
)
