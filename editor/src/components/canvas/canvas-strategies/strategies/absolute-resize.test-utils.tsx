import type { Delta } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import type { EdgePosition } from '../../canvas-types'
import { mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'

export function getResizeControl(
  renderResult: EditorRenderResult,
  edgePosition: EdgePosition,
): HTMLElement | null {
  const testIdOfControl = `resize-control-${edgePosition.x}-${edgePosition.y}`
  return renderResult.renderedDOM.queryByTestId(testIdOfControl)
}

export async function resizeElement(
  renderResult: EditorRenderResult,
  dragDelta: Delta,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
  options: {
    midDragCallback?: () => Promise<void>
  } = {},
): Promise<void> {
  const canvasControl = getResizeControl(renderResult, edgePosition)
  if (canvasControl == null) {
    throw new Error(`Could not find canvas control.`)
  }

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  await mouseDragFromPointWithDelta(canvasControl, startPoint, dragDelta, {
    modifiers: modifiers,
    midDragCallback: options.midDragCallback,
  })
}
