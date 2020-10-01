import * as React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { colorTheme } from 'uuiui'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'

interface HighlightControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color?: string
  striped?: boolean
  zOffset?: number | null
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
            backgroundImage: this.props.striped
              ? `linear-gradient(135deg, ${this.props.color} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${this.props.color} 50%, ${this.props.color} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`
              : '',
            backgroundSize: `${20 / this.props.scale}px ${20 / this.props.scale}px`,
            pointerEvents: 'none',
            borderRadius: borderRadius,
            transform: this.props.zOffset
              ? `translate3d(0, 0, ${this.props.zOffset || 0}px)`
              : 'none',
            transformStyle: this.props.zOffset ? 'preserve-3d' : undefined,
          }}
        />
      </>
    )
  }
}
