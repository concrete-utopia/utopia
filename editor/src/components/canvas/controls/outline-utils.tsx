import type { CanvasRectangle } from '../../../core/shared/math-utils'

export const ZeroControlSize = 5
export function isZeroSizedElement(frame: CanvasRectangle): boolean {
  return frame.width === 0 || frame.height === 0
}
