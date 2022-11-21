import { CanvasPoint, canvasPoint, roundTo, Size } from '../../core/shared/math-utils'
import { assertNever } from '../../core/shared/utils'
import { CSSBorderRadiusIndividual } from '../inspector/common/css-utils'

export type BorderRadiusCorner = keyof CSSBorderRadiusIndividual

export const BorderRadiusCorners: BorderRadiusCorner[] = ['tl', 'tr', 'bl', 'br']

export type BorderRadiusSides<T> = { [key in BorderRadiusCorner]: T }

export const BorderRadiusControlMinimumForDisplay = (scale: number): number => 12 / scale

export type BorderRadiusAdjustMode = 'individual' | 'all'

export const BorderRadiusHandleSize = (scale: number): number => 8 / scale

export const BorderRadiusHandleDotSize = (scale: number): number => 2 / scale

export const BorderRadiusHandleBorderWidth = (scale: number): number => 1 / scale

export const BorderRadiusHandleHitArea = (scale: number): number => 20 / scale

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
