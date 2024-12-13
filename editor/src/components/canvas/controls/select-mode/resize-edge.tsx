import React from 'react'
import type { CSSCursor, EdgePosition } from '../../canvas-types'
import { ResizePointTestId } from './absolute-resize-control'

interface ResizeEdgeProps {
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  position: EdgePosition
  onMouseDown: (e: React.MouseEvent<HTMLDivElement>) => void
  onMouseMove: (e: React.MouseEvent<HTMLDivElement>) => void
  onDoubleClick: (e: React.MouseEvent<HTMLDivElement>) => void
}

export const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          backgroundColor: 'transparent',
          cursor: props.cursor,
          pointerEvents: 'initial',
        }}
        onMouseDown={props.onMouseDown}
        onMouseMove={props.onMouseMove}
        onDoubleClick={props.onDoubleClick}
        data-testid={ResizePointTestId(props.position)}
      />
    )
  }),
)
ResizeEdge.displayName = 'ResizeEdge'
