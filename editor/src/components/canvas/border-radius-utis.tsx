import { CanvasPoint, canvasPoint, roundTo, Size } from '../../core/shared/math-utils'
import { CSSBorderRadiusIndividual } from '../inspector/common/css-utils'
import {
  EdgePosition,
  EdgePositionTopLeft,
  EdgePositionTopRight,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
} from './canvas-types'
import { CSSNumberWithRenderedValue } from './controls/select-mode/controls-common'

export type BorderRadiusAdjustMode = 'individual' | 'all'

export type BorderRadiusSides<T> = { [key in keyof CSSBorderRadiusIndividual]: T }

export const BorderRadiusHandleSize = (scale: number): number => 12 / scale

export const BorderRadiusThreshold: number = 10

export const borderRadiusOffsetPx = (isDragging: boolean, desiredOffset: number): number =>
  isDragging ? desiredOffset : Math.max(desiredOffset, BorderRadiusThreshold)

export const maxBorderRadius = (elementSize: Size): number =>
  roundTo(Math.min(elementSize.height, elementSize.width) / 2, 0)

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

// this whole `EdgePosition` business is instant legacy, `keyof CSSBorderRadiusIndividual` should be used instead
// TODO delete me before merge!
export function valueFromEdgePosition(
  edgePosition: EdgePosition,
  borderRadius: BorderRadiusSides<CSSNumberWithRenderedValue>,
): CSSNumberWithRenderedValue {
  const { x, y } = edgePosition
  if (x === EdgePositionTopLeft.x && y === EdgePositionTopLeft.y) {
    return borderRadius.tl
  }

  if (x === EdgePositionTopRight.x && y === EdgePositionTopRight.y) {
    return borderRadius.tr
  }

  if (x === EdgePositionBottomLeft.x && y === EdgePositionBottomLeft.y) {
    return borderRadius.bl
  }

  if (x === EdgePositionBottomRight.x && y === EdgePositionBottomRight.y) {
    return borderRadius.br
  }

  return borderRadius.tl // ughh
}
