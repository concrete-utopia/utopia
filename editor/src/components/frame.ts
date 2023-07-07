import Utils from '../utils/utils'
import type { LocalRectangle } from '../core/shared/math-utils'
import { CoordinateMarker, Rectangle } from '../core/shared/math-utils'

export type FullFrame = {
  left: number
  right: number
  centerX: number
  top: number
  bottom: number
  centerY: number
  width: number
  height: number
}

export function getFullFrame(rectangle: LocalRectangle): FullFrame {
  return {
    left: rectangle.x,
    centerX: rectangle.x + rectangle.width / 2,
    right: rectangle.x + rectangle.width,
    width: rectangle.width,
    top: rectangle.y,
    centerY: rectangle.y + rectangle.height / 2,
    bottom: rectangle.y + rectangle.height,
    height: rectangle.height,
  }
}
