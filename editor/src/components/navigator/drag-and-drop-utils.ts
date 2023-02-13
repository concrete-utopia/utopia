export interface CollectResults {
  isDragging: boolean
  isOver: boolean
}

const OuterHoverThreshold = 20

export function isCursorInTopArea(
  rectangle: ClientRect,
  cursorY: number,
  numberOfAreasToCut: number,
): boolean {
  return rectangle.top + OuterHoverThreshold > cursorY
}

export function isCursorInBottomArea(
  rectangle: ClientRect,
  cursorY: number,
  numberOfAreasToCut: number,
): boolean {
  return rectangle.bottom - OuterHoverThreshold < cursorY
}
