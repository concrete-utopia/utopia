import React from 'react'
import type { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { isZeroSizedElement } from './outline-utils'
import { ZeroSizeHighlightControl } from './zero-sized-element-controls'

interface HighlightControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color?: string
  displayZeroSized: 'display-zero-sized' | 'do-not-display-zero-sized'
}

export const HighlightControl = React.memo((props: HighlightControlProps) => {
  const colorTheme = useColorTheme()
  const outlineWidth = 1.5 / props.scale
  const outlineColor =
    props.color === null ? colorTheme.canvasSelectionPrimaryOutline.value : props.color

  if (isZeroSizedElement(props.frame)) {
    // Only display the zero size higlight if it should be displayed.
    if (props.displayZeroSized === 'display-zero-sized') {
      return (
        <ZeroSizeHighlightControl
          frame={props.frame}
          canvasOffset={props.canvasOffset}
          scale={props.scale}
          color={outlineColor}
        />
      )
    } else {
      return null
    }
  } else {
    return (
      <div
        className='role-component-highlight-outline'
        data-testid='highlight-control'
        style={{
          position: 'absolute',
          left: props.canvasOffset.x + props.frame.x,
          top: props.canvasOffset.y + props.frame.y,
          width: props.frame.width,
          height: props.frame.height,
          boxShadow: `0px 0px 0px ${outlineWidth}px ${outlineColor}`,
          pointerEvents: 'none',
        }}
      />
    )
  }
})
