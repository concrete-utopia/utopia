import React from 'react'
import { canvasPoint, CanvasPoint, CanvasVector } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { EdgePiece } from '../../canvas-types'
import { deltaFromEdge } from '../../padding-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const PaddingValueIndicatorTestId = 'PaddingValueIndicatorTestId'

export interface PaddingValueIndicatorProps {
  currentPaddingValue: number
  activeEdge: EdgePiece
  dragDelta: CanvasVector
  dragStart: CanvasPoint
}

export const PaddingValueIndicator = controlForStrategyMemoized<PaddingValueIndicatorProps>(
  (props) => {
    const { currentPaddingValue, activeEdge, dragStart, dragDelta } = props
    const colorTheme = useColorTheme()

    const actualPaddingValue = Math.max(
      0,
      currentPaddingValue + deltaFromEdge(dragDelta, activeEdge),
    )
    const position = indicatorPosition(activeEdge, dragStart, dragDelta)
    return (
      <CanvasOffsetWrapper>
        <div
          data-testid={PaddingValueIndicatorTestId}
          style={{
            position: 'absolute',
            left: position.x,
            top: position.y,
            backgroundColor: colorTheme.brandNeonPink.value,
            color: 'white',
          }}
        >
          {actualPaddingValue}
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

function indicatorPosition(
  edge: EdgePiece,
  dragStart: CanvasPoint,
  dragDelta: CanvasVector,
): CanvasPoint {
  const Offset = 5
  switch (edge) {
    case 'top':
    case 'bottom':
      return canvasPoint({ x: dragStart.x + Offset, y: dragStart.y + dragDelta.y + Offset })
    case 'left':
    case 'right':
      return canvasPoint({ x: dragStart.x + dragDelta.x + Offset, y: dragStart.y + Offset })
    default:
      assertNever(edge)
  }
}
