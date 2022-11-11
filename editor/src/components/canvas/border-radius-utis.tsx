import { CanvasPoint, canvasPoint, roundTo, Size } from '../../core/shared/math-utils'
import { assertNever } from '../../core/shared/utils'
import { CSSBorderRadiusIndividual } from '../inspector/common/css-utils'

export type BorderRadiusCorner = keyof CSSBorderRadiusIndividual

export const BorderRadiusCorners: BorderRadiusCorner[] = ['tl', 'tr', 'bl', 'br']

export type BorderRadiusSides<T> = { [key in BorderRadiusCorner]: T }

export type BorderRadiusAdjustMode = 'individual' | 'all'

export const BorderRadiusHandleSize = (scale: number): number => 12 / scale

export const BorderRadiusThreshold: number = 10

export const borderRadiusOffsetPx = (isDragging: boolean, desiredOffset: number): number =>
  isDragging ? desiredOffset : Math.max(desiredOffset, BorderRadiusThreshold)

export const maxBorderRadius = (elementSize: Size): number =>
  roundTo(Math.min(elementSize.height, elementSize.width) / 2, 0)

export function handlePosition(
  offset: number,
  elementSize: Size,
  corner: BorderRadiusCorner,
  scale: number,
): CanvasPoint {
  const handleSize = BorderRadiusHandleSize(scale) / 2
  switch (corner) {
    case 'tl':
      return canvasPoint({ x: offset - handleSize, y: offset - handleSize })
    case 'tr':
      return canvasPoint({ x: elementSize.width - offset - handleSize, y: offset - handleSize })
    case 'bl':
      return canvasPoint({ x: offset - handleSize, y: elementSize.height - offset - handleSize })
    case 'br':
      return canvasPoint({
        x: elementSize.width - offset - handleSize,
        y: elementSize.height - offset - handleSize,
      })
    default:
      assertNever(corner)
  }
}
