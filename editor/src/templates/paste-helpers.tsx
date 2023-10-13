import { IS_TEST_ENVIRONMENT } from '../common/env-vars'
import type { CanvasVector, Size } from '../core/shared/math-utils'
import { canvasPoint } from '../core/shared/math-utils'

/**
 * Calculates the point on the canvas that corresponds to the visual center of the active scrolled, zoomed viewport.
 * Useful if you want to paste something right in the middle of the visible Canvas area
 */
export function getCanvasViewportCenter(
  canvasOffset: CanvasVector,
  canvasScale: number,
  canvasWrapperRect: Size,
): CanvasVector {
  const result = canvasPoint({
    x: -canvasOffset.x + canvasWrapperRect.width / canvasScale / 2,
    y: -canvasOffset.y + canvasWrapperRect.height / canvasScale / 2,
  })

  /**
   * For tests, we want this canvas centerpoint to be stable so it's not sensitive to the canvas default scroll changing, or UI chrome changes around the canvas
   */
  if (IS_TEST_ENVIRONMENT) {
    // should this be mocked? is a mock easier to debug, or just obfuscating the fact that the tests have special behavior?
    return canvasPoint({ x: 719.5, y: 420 }) // these were the numbers when the original paste tests were written. TODO replace with a cleaner number and change the paste test values
  }

  return result
}
