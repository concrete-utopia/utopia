import * as React from 'react'

import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { colorTheme } from '../../../uuiui'
import { GuidelineWithSnappingVector } from '../guideline'

interface ResizeEdgeProps {
  canvasOffset: CanvasPoint
  scale: number
  guidelineWithSnapping: GuidelineWithSnappingVector
  targetFrame: CanvasRectangle
}

export function GuidelineControl(props: ResizeEdgeProps) {
  const { targetFrame } = props
  const guideline = props.guidelineWithSnapping.guideline
  const lineSize = 1 / props.scale

  let rectangle: {
    left: number
    top: number
    right: number
    bottom: number
  }
  switch (guideline.type) {
    case 'XAxisGuideline':
      const x =
        guideline.x === targetFrame.x + targetFrame.width ? guideline.x - lineSize : guideline.x
      rectangle = {
        left: x,
        top: Math.min(guideline.yTop, targetFrame.y),
        right: x,
        bottom: Math.max(guideline.yBottom, targetFrame.y + targetFrame.height),
      }
      break
    case 'YAxisGuideline':
      const y =
        guideline.y === targetFrame.y + targetFrame.height ? guideline.y - lineSize : guideline.y
      rectangle = {
        left: Math.min(guideline.xLeft, targetFrame.x),
        top: y,
        right: Math.max(guideline.xRight, targetFrame.x + targetFrame.width),
        bottom: y,
      }
      break
    case 'CornerGuideline':
      rectangle = {
        left: guideline.x,
        top: Math.min(guideline.y + guideline.yMovement, targetFrame.y),
        right: guideline.x,
        bottom: Math.max(guideline.y + guideline.yMovement, targetFrame.y + targetFrame.height),
      }
      rectangle = {
        left: Math.min(guideline.x + guideline.xMovement, targetFrame.x),
        top: guideline.y,
        right: Math.max(guideline.x + guideline.xMovement, targetFrame.x + targetFrame.width),
        bottom: guideline.y,
      }
      break
    default:
      const _exhaustiveCheck: never = guideline
      throw 'Unexpected value for guideline: ' + guideline
  }

  const width = rectangle.right - rectangle.left
  const height = rectangle.bottom - rectangle.top
  const horizontal = width > height
  const haypixelOffset = -0.5 / props.scale

  return (
    <div
      style={{
        pointerEvents: 'none',
        position: 'absolute',
        left: props.canvasOffset.x + rectangle.left,
        top: props.canvasOffset.y + rectangle.top,
        width,
        height,
        boxSizing: 'border-box',
        transform: `translate(${horizontal ? 0 : haypixelOffset}px, ${
          horizontal ? haypixelOffset : 0
        }px)`,
        borderWidth: 0,
        borderLeftWidth: lineSize,
        borderTopWidth: lineSize,
        borderStyle: props.guidelineWithSnapping.activateSnap ? 'solid' : 'dashed',
        borderColor: colorTheme.canvasLayoutStroke.value,
      }}
    />
  )
}
