import * as React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'

interface HighlightControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color?: string
}

export const HighlightControl = betterReactMemo(
  'HighlightControl',
  (props: HighlightControlProps) => {
    const colorTheme = useColorTheme()
    const outlineWidth = 1.5 / props.scale
    const outlineColor =
      props.color === null ? colorTheme.canvasSelectionPrimaryOutline.value : props.color

    const { borderRadius, extraWidth, extraHeight } = calculateExtraSizeForZeroSizedElement(
      props.frame,
    )

    return (
      <>
        <div
          className='role-component-highlight-outline'
          style={{
            position: 'absolute',
            left: props.canvasOffset.x + props.frame.x - extraWidth / 2,
            top: props.canvasOffset.y + props.frame.y - extraHeight / 2,
            width: props.frame.width + extraWidth,
            height: props.frame.height + extraHeight,
            boxShadow: `0px 0px 0px ${outlineWidth}px ${outlineColor}`,
            pointerEvents: 'none',
            borderRadius: borderRadius,
          }}
        />
      </>
    )
  },
)
