import * as React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { colorTheme } from 'uuiui'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'

interface HighlightControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color?: string
}

export class HighlightControl extends React.Component<HighlightControlProps> {
  render() {
    const outlineWidth = 1.5 / this.props.scale
    const outlineColor =
      this.props.color === null ? colorTheme.canvasSelectionPrimaryOutline.value : this.props.color

    const { borderRadius, extraWidth, extraHeight } = calculateExtraSizeForZeroSizedElement(
      this.props.frame,
    )

    return (
      <>
        <div
          className='role-component-highlight-outline'
          style={{
            position: 'absolute',
            left: this.props.canvasOffset.x + this.props.frame.x - extraWidth / 2,
            top: this.props.canvasOffset.y + this.props.frame.y - extraHeight / 2,
            width: this.props.frame.width + extraWidth,
            height: this.props.frame.height + extraHeight,
            boxShadow: `0px 0px 0px ${outlineWidth}px ${outlineColor}`,
            pointerEvents: 'none',
            borderRadius: borderRadius,
          }}
        />
      </>
    )
  }
}
