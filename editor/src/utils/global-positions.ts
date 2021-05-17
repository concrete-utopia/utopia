import { CanvasPoint, WindowPoint } from '../core/shared/math-utils'

export let CanvasMousePositionRaw: CanvasPoint | null = null
export let CanvasMousePositionRounded: CanvasPoint | null = null
export let WindowMousePositionRaw: WindowPoint | null = null

export function updateGlobalPositions(
  canvasMousePositionRaw: CanvasPoint,
  canvasMousePositionRounded: CanvasPoint,
  windowMousePositionRaw: WindowPoint,
): void {
  CanvasMousePositionRaw = canvasMousePositionRaw
  CanvasMousePositionRounded = canvasMousePositionRounded
  WindowMousePositionRaw = windowMousePositionRaw
}
