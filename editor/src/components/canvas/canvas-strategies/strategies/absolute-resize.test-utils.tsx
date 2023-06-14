import { Delta, windowPoint } from '../../../../core/shared/math-utils'
import { Modifiers } from '../../../../utils/modifiers'
import { EdgePosition } from '../../canvas-types'
import { mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { EditorRenderResult } from '../../ui-jsx.test-utils'

export async function resizeElement(
  renderResult: EditorRenderResult,
  dragDelta: Delta,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
): Promise<void> {
  const canvasControl = renderResult.renderedDOM.queryByTestId(
    `resize-control-${edgePosition.x}-${edgePosition.y}`,
  )
  if (canvasControl == null) {
    return
  }

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  await mouseDragFromPointWithDelta(canvasControl, startPoint, dragDelta, { modifiers: modifiers })
}
