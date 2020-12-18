import * as React from 'react'
import { CanvasVector, CanvasRectangle } from '../../../core/shared/math-utils'
import { colorTheme } from '../../../uuiui'
import { Outline } from './outline'

export interface InsertionControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasVector
  scale: number
}

export class InsertionControls extends React.Component<InsertionControlProps> {
  render() {
    // TODO Show transparent image overlay when inserting an image
    return (
      <Outline
        rect={this.props.frame}
        offset={this.props.canvasOffset}
        scale={this.props.scale}
        color={colorTheme.canvasSelectionSecondaryOutline.value}
      />
    )
  }
}
