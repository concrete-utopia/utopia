import { CanvasRectangle } from '../../../core/shared/math-utils'

export const ZeroControlSize = 5
export function isZeroSizedElement(frame: CanvasRectangle | null | undefined): boolean {
  if (frame == null) {
    return false
  } else {
    return frame.width === 0 || frame.height === 0
  }
}
