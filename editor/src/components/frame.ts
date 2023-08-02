import Utils from '../utils/utils'
import type { LocalRectangle } from '../core/shared/math-utils'
import { CoordinateMarker, Rectangle } from '../core/shared/math-utils'

export type FullFrame = {
  left: number
  right: number
  top: number
  bottom: number
  width: number
  height: number
}

export function getFullFrame(rectangle: LocalRectangle): FullFrame {
  return {
    left: rectangle.x,
    right: rectangle.x + rectangle.width,
    width: rectangle.width,
    top: rectangle.y,
    bottom: rectangle.y + rectangle.height,
    height: rectangle.height,
  }
}
