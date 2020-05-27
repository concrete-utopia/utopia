import * as React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'

interface OutlineProps {
  offset: CanvasPoint
  scale: number
  rect: CanvasRectangle
  color: string
  striped?: boolean
  stripedColor?: string
}

export class Outline extends React.Component<OutlineProps> {
  render() {
    const outlineWidth = 1 / this.props.scale

    // outline pushed inside the rectangle, shifted with 0.5px from left and 0.5px from right at 100% zoom.
    const outlineOffset = 0.5 / this.props.scale
    const outlineWidthHeightOffset = -outlineOffset * 3
    const stripedColor =
      this.props.stripedColor === undefined ? this.props.color : this.props.stripedColor

    return (
      <div
        className='role-outline'
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          left: this.props.offset.x + this.props.rect.x + outlineOffset,
          top: this.props.offset.y + this.props.rect.y + outlineOffset,
          width: this.props.rect.width + outlineWidthHeightOffset,
          height: this.props.rect.height + outlineWidthHeightOffset,
          boxShadow: `0px 0px 0px ${outlineWidth}px ${this.props.color}`,
          backgroundImage: this.props.striped
            ? `linear-gradient(135deg, ${stripedColor} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${stripedColor} 50%, ${this.props.color} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`
            : '',
          backgroundSize: `${20 / this.props.scale}px ${20 / this.props.scale}px`,
          pointerEvents: 'none',
        }}
      />
    )
  }
}
