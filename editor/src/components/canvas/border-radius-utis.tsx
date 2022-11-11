import { CanvasPoint, canvasPoint, Size } from '../../core/shared/math-utils'
import {
  EdgePosition,
  EdgePositionTopLeft,
  EdgePositionTopRight,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
} from './canvas-types'

export const BorderRadiusHandleSize = (scale: number): number => 12 / scale

export const BorderRadiusThreshold: number = 10

export const borderRadiusOffsetPx = (isDragging: boolean, desiredOffset: number): number =>
  isDragging ? desiredOffset : Math.max(desiredOffset, BorderRadiusThreshold)

export function handlePosition(
  offset: number,
  elementSize: Size,
  edgePosition: EdgePosition,
  scale: number,
): CanvasPoint {
  const handleSize = BorderRadiusHandleSize(scale) / 2

  const { x, y } = edgePosition
  if (x === EdgePositionTopLeft.x && y === EdgePositionTopLeft.y) {
    return canvasPoint({ x: offset - handleSize, y: offset - handleSize })
  }

  if (x === EdgePositionTopRight.x && y === EdgePositionTopRight.y) {
    return canvasPoint({ x: elementSize.width - offset - handleSize, y: offset - handleSize })
  }

  if (x === EdgePositionBottomLeft.x && y === EdgePositionBottomLeft.y) {
    return canvasPoint({ x: offset - handleSize, y: elementSize.height - offset - handleSize })
  }

  if (x === EdgePositionBottomRight.x && y === EdgePositionBottomRight.y) {
    return canvasPoint({
      x: elementSize.width - offset - handleSize,
      y: elementSize.height - offset - handleSize,
    })
  }

  return canvasPoint({ x: 0, y: 0 })
}
