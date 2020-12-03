import * as React from 'react'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'

interface OutlineProps {
  offset: CanvasPoint
  scale: number
  rect: CanvasRectangle
  color: string
  striped?: boolean
  stripedColor?: string
  zOffset?: number | null
}

export class Outline extends React.Component<OutlineProps> {
  render() {
    const outlineWidth = 1 / this.props.scale

    // outline pushed inside the rectangle, shifted with 0.5px from left and 0.5px from right at 100% zoom.
    const outlineOffset = 0.5 / this.props.scale
    const outlineWidthHeightOffset = -outlineOffset * 3
    const stripedColor =
      this.props.stripedColor === undefined ? this.props.color : this.props.stripedColor

    const { borderRadius, extraWidth, extraHeight } = calculateExtraSizeForZeroSizedElement(
      this.props.rect,
    )

    return (
      <div
        className='role-outline'
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          left: this.props.offset.x + this.props.rect.x + outlineOffset - extraWidth / 2,
          top: this.props.offset.y + this.props.rect.y + outlineOffset - extraHeight / 2,
          width: this.props.rect.width + outlineWidthHeightOffset + extraWidth,
          height: this.props.rect.height + outlineWidthHeightOffset + extraHeight,
          boxShadow: `0px 0px 0px ${outlineWidth}px ${this.props.color}`,
          backgroundImage: this.props.striped
            ? `linear-gradient(135deg, ${stripedColor} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${stripedColor} 50%, ${this.props.color} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`
            : '',
          backgroundSize: `${20 / this.props.scale}px ${20 / this.props.scale}px`,
          borderRadius: borderRadius,
          pointerEvents: 'none',
          transform: this.props.zOffset
            ? `translate3d(0, 0, ${this.props.zOffset || 0}px)`
            : 'none',
          transformStyle: this.props.zOffset ? 'preserve-3d' : undefined,
        }}
      />
    )
  }
}
