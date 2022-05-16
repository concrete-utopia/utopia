export interface CollectResults {
  isDragging: boolean
  isOver: boolean
}

export function isCursorInTopArea(
  rectangle: ClientRect,
  cursorY: number,
  numberOfAreasToCut: number,
): boolean {
  return rectangle.top + rectangle.height / numberOfAreasToCut > cursorY
}

export function isCursorInBottomArea(
  rectangle: ClientRect,
  cursorY: number,
  numberOfAreasToCut: number,
): boolean {
  return rectangle.bottom - rectangle.height / numberOfAreasToCut < cursorY
}
