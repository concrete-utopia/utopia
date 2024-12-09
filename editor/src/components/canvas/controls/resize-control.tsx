import * as React from 'react'
import type { EdgePosition, EdgePositionCorner } from '../canvas-types'
import { CSSCursor } from '../canvas-types'
import type { ElementPath } from 'utopia-shared/src/types'
import { useResizeEdges } from './select-mode/use-resize-edges'
import { NO_OP } from '../../../core/shared/utils'
import { useColorTheme } from '../../../uuiui'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { isEdgePositionEqualTo } from '../canvas-utils'
import { useBoundingBox } from './bounding-box-hooks'

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
  onPointMouseDown?: (event: React.MouseEvent<HTMLDivElement>, position: EdgePosition) => void
  onPointDoubleClick?: (event: React.MouseEvent<HTMLDivElement>) => void
}

export const ResizePointTestId = (position: EdgePosition): string =>
  `resize-control-${position.x}-${position.y}`
const ResizePointMouseAreaSize = 12
const ResizePointMouseAreaOffset = ResizePointMouseAreaSize / 2
const ResizePointSize = 6
const ResizePointOffset = ResizePointSize / 2
const ResizePoint = React.memo(
  React.forwardRef<HTMLDivElement, ResizePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ResizeEdge scale',
    )

    const onPointMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        if (props.onPointMouseDown != null) {
          props.onPointMouseDown(event, props.position)
        }
      },
      [props],
    )

    const onMouseMove = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        maybeClearHighlightsOnHoverEnd()
        event.stopPropagation()
      },
      [maybeClearHighlightsOnHoverEnd],
    )

    const hiddenDuringInteraction = useEditorState(
      Substores.canvas,
      (store) =>
        store.editor.canvas.interactionSession != null &&
        store.editor.canvas.interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
        !isEdgePositionEqualTo(
          props.position,
          store.editor.canvas.interactionSession.activeControl.edgePosition,
        ),
      'ResizePoint hiddenDuringInteraction',
    )

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          visibility: hiddenDuringInteraction ? 'hidden' : 'visible',
        }}
      >
        <div
          style={{
            position: 'relative',
            width: ResizePointSize / scale,
            height: ResizePointSize / scale,
            top: -ResizePointOffset / scale,
            left: -ResizePointOffset / scale,
            boxSizing: 'border-box',
            borderWidth: 1 / scale,
            backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor50.value} 0px 0px
              ${1 / scale}px, ${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px ${
              1 / scale
            }px ${2 / scale}px ${1 / scale}px`,
          }}
        />
        <div
          style={{
            position: 'relative',
            width: ResizePointMouseAreaSize / scale,
            height: ResizePointMouseAreaSize / scale,
            top: -ResizePointMouseAreaSize / scale,
            left: -ResizePointMouseAreaOffset / scale,
            backgroundColor: 'transparent',
            pointerEvents: 'initial',
            cursor: props.cursor,
          }}
          onDoubleClick={props.onPointDoubleClick}
          onMouseDown={onPointMouseDown}
          onMouseMove={onMouseMove}
          data-testid={ResizePointTestId(props.position)}
        />
      </div>
    )
  }),
)
ResizePoint.displayName = 'ResizePoint'

interface ResizeControlProps {
  ['data-testid']?: string
  position: React.CSSProperties['position']
  expandToFill: 'expand' | 'do-not-expand'
  targets: Array<ElementPath>
  onEdgeMouseDown?: (event: React.MouseEvent<HTMLDivElement>, position: EdgePosition) => void
  onEdgeMouseMove?: (event: React.MouseEvent<HTMLDivElement>) => void
  onEdgeDoubleClick?: (
    event: React.MouseEvent<HTMLDivElement>,
    direction: 'horizontal' | 'vertical',
  ) => void
  onCornerMouseDown?: (
    event: React.MouseEvent<HTMLDivElement>,
    position: EdgePositionCorner,
  ) => void
  onCornerDoubleClick?: (event: React.MouseEvent<HTMLDivElement>) => void
  edgeCursors?: {
    top?: CSSCursor
    left?: CSSCursor
    bottom?: CSSCursor
    right?: CSSCursor
  }
}

export const ResizeControl = React.memo(
  React.forwardRef<HTMLDivElement, ResizeControlProps>((props, ref) => {
    const resizeEdges = useResizeEdges(props.targets, {
      onEdgeDoubleClick: props.onEdgeDoubleClick ?? NO_OP,
      onEdgeMouseMove: props.onEdgeMouseMove ?? NO_OP,
      onEdgeMouseDown: props.onEdgeMouseDown ?? NO_OP,
      cursors: props.edgeCursors,
    })

    const topLeftRef = useBoundingBox(props.targets, NO_OP)
    const topRightRef = useBoundingBox(props.targets, (boundingRef, boundingBox) => {
      boundingRef.current.style.left = boundingBox.width + 'px'
    })
    const bottomLeftRef = useBoundingBox(props.targets, (boundingRef, boundingBox) => {
      boundingRef.current.style.top = boundingBox.height + 'px'
    })
    const bottomRightRef = useBoundingBox(props.targets, (boundingRef, boundingBox) => {
      boundingRef.current.style.left = boundingBox.width + 'px'
      boundingRef.current.style.top = boundingBox.height + 'px'
    })

    return (
      <div
        data-testid={props['data-testid']}
        ref={ref}
        style={{
          position: props.position,
          width: props.expandToFill === 'expand' ? '100%' : undefined,
          height: props.expandToFill === 'expand' ? '100%' : undefined,
          pointerEvents: 'none',
        }}
      >
        {resizeEdges.top}
        {resizeEdges.bottom}
        {resizeEdges.left}
        {resizeEdges.right}
        <ResizePoint
          ref={topLeftRef}
          position={{ x: 0, y: 0 }}
          cursor={CSSCursor.ResizeNWSE}
          onPointMouseDown={props.onCornerMouseDown}
          onPointDoubleClick={props.onCornerDoubleClick}
        />
        <ResizePoint
          ref={topRightRef}
          position={{ x: 1, y: 0 }}
          cursor={CSSCursor.ResizeNESW}
          onPointMouseDown={props.onCornerMouseDown}
          onPointDoubleClick={props.onCornerDoubleClick}
        />
        <ResizePoint
          ref={bottomLeftRef}
          position={{ x: 0, y: 1 }}
          cursor={CSSCursor.ResizeNESW}
          onPointMouseDown={props.onCornerMouseDown}
          onPointDoubleClick={props.onCornerDoubleClick}
        />
        <ResizePoint
          ref={bottomRightRef}
          position={{ x: 1, y: 1 }}
          cursor={CSSCursor.ResizeNWSE}
          onPointMouseDown={props.onCornerMouseDown}
          onPointDoubleClick={props.onCornerDoubleClick}
        />
      </div>
    )
  }),
)
ResizeControl.displayName = 'ResizeControl'
