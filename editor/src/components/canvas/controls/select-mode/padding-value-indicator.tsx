import React from 'react'
import { canvasPoint, CanvasPoint, CanvasVector } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
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

const FontSize = 12
const Padding = 4

interface PaddingValueLabelProps {
  scale: number
  color: string
  value: number
}

export const PaddingValueLabel: React.FC<PaddingValueLabelProps> = (props) => {
  const { scale, color, value } = props
  const fontSize = FontSize / scale
  const padding = Padding / scale
  return (
    <div
      style={{
        fontSize: fontSize,
        padding: padding,
        backgroundColor: color,
        color: 'white',
      }}
    >
      {value}
    </div>
  )
}

export const PaddingValueIndicator = controlForStrategyMemoized<PaddingValueIndicatorProps>(
  (props) => {
    const { currentPaddingValue, activeEdge, dragStart, dragDelta } = props
    const colorTheme = useColorTheme()
    const scale = useEditorState(
      (store) => store.editor.canvas.scale,
      'PaddingValueIndicator scale',
    )

    const actualPaddingValue = Math.floor(
      Math.max(0, currentPaddingValue + deltaFromEdge(dragDelta, activeEdge)),
    )
    const position = indicatorPosition(activeEdge, scale, dragStart, dragDelta)
    return (
      <CanvasOffsetWrapper>
        <div
          data-testid={PaddingValueIndicatorTestId}
          style={{
            position: 'absolute',
            left: position.x,
            top: position.y,
          }}
        >
          <PaddingValueLabel
            value={actualPaddingValue}
            scale={scale}
            color={colorTheme.brandNeonPink.value}
          />
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

function indicatorPosition(
  edge: EdgePiece,
  scale: number,
  dragStart: CanvasPoint,
  dragDelta: CanvasVector,
): CanvasPoint {
  const Offset = 4 / scale
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
