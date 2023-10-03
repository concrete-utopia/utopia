import React from 'react'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { isZeroSizedElement } from './outline-utils'

interface TextEditableControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
}

export const TextEditableControl = React.memo((props: TextEditableControlProps) => {
  const colorTheme = useColorTheme()
  const outlineWidth = 1.5 / props.scale
  const outlineColor = colorTheme.textEditableOutline.value

  if (isZeroSizedElement(props.frame)) {
    return null
  }

  return (
    <div
      className='role-component-highlight-text-editable'
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
})
