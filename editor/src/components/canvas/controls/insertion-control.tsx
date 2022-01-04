import React from 'react'
import { CanvasVector, CanvasRectangle } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { Outline } from './outline'

export interface InsertionControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasVector
  scale: number
}

export const InsertionControls = React.memo((props: InsertionControlProps) => {
  const colorTheme = useColorTheme()
  // TODO Show transparent image overlay when inserting an image
  return (
    <Outline
      rect={props.frame}
      offset={props.canvasOffset}
      scale={props.scale}
      color={colorTheme.canvasSelectionSecondaryOutline.value}
    />
  )
})
