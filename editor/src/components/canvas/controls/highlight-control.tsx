import * as React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { colorTheme } from 'uuiui'

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
    return (
      <>
        <div
          className='role-component-highlight-outline'
          style={{
            position: 'absolute',
            left: this.props.canvasOffset.x + this.props.frame.x,
            top: this.props.canvasOffset.y + this.props.frame.y,
            width: this.props.frame.width,
            height: this.props.frame.height,
            boxShadow: `0px 0px 0px ${outlineWidth}px ${outlineColor}`,
            pointerEvents: 'none',
          }}
        />
      </>
    )
  }
}
