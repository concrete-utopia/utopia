import React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { useRoundedCanvasOffset } from '../canvas-atoms'
import { isZeroSizedElement, ZeroControlSize } from './outline-utils'
import { ZeroSizeHighlightControl } from './zero-sized-element-controls'

interface HighlightControlProps {
  frame: CanvasRectangle
  scale: number
  color?: string
}

export const HighlightControl = React.memo((props: HighlightControlProps) => {
  const [canvasOffset] = useRoundedCanvasOffset() // TODO do we need this, or can we use css-var?
  const colorTheme = useColorTheme()
  const outlineWidth = 1.5 / props.scale
  const outlineColor =
    props.color === null ? colorTheme.canvasSelectionPrimaryOutline.value : props.color

  if (isZeroSizedElement(props.frame)) {
    return (
      <ZeroSizeHighlightControl
        frame={props.frame}
        canvasOffset={canvasOffset}
        scale={props.scale}
        color={outlineColor}
      />
    )
  } else {
    return (
      <div
        className='role-component-highlight-outline'
        style={{
          position: 'absolute',
          left: canvasOffset.x + props.frame.x,
          top: canvasOffset.y + props.frame.y,
          width: props.frame.width,
          height: props.frame.height,
          boxShadow: `0px 0px 0px ${outlineWidth}px ${outlineColor}`,
          pointerEvents: 'none',
        }}
      />
    )
  }
})
