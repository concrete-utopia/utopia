import { getRectCenter, localRectangle } from '../../../../core/shared/math-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'
import { GridCellTestId } from '../../controls/grid-controls-for-strategies'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseDownAtPoint,
  mouseMoveToPoint,
  keyDown,
  mouseUpAtPoint,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import type { GridCellCoordinates } from './grid-cell-bounds'
import { gridCellTargetId } from './grid-cell-bounds'
import * as EP from '../../../../core/shared/element-path'

export async function runGridMoveTest(
  editor: EditorRenderResult,
  props: {
    scale: number
    pathString: string
    testId: string
    targetCell?: GridCellCoordinates
    draggedCell?: GridCellCoordinates
    tab?: boolean
  },
  midDragCallback?: (editor: EditorRenderResult) => void,
) {
  const elementPathToDrag = EP.fromString(props.pathString)

  await selectComponentsForTest(editor, [elementPathToDrag])

  if (props.scale !== 1) {
    await editor.dispatch([CanvasActions.zoom(props.scale)], true)
  }

  const sourceGridCell = editor.renderedDOM.getByTestId(
    props.draggedCell == null
      ? GridCellTestId(elementPathToDrag)
      : gridCellTargetId(
          EP.fromString('sb/scene/grid'),
          props.draggedCell.row,
          props.draggedCell.column,
        ),
  )
  const targetGridCell = editor.renderedDOM.getByTestId(
    gridCellTargetId(
      EP.fromString('sb/scene/grid'),
      props.targetCell?.row ?? 2,
      props.targetCell?.column ?? 3,
    ),
  )

  const sourceRect = sourceGridCell.getBoundingClientRect()
  const targetRect = targetGridCell.getBoundingClientRect()

  const dragFrom = {
    x: sourceRect.x + 10,
    y: sourceRect.y + 10,
  }
  const endPoint = getRectCenter(
    localRectangle({
      x: targetRect.x,
      y: targetRect.y,
      width: targetRect.width,
      height: targetRect.height,
    }),
  )

  await mouseDownAtPoint(sourceGridCell, dragFrom)
  await mouseMoveToPoint(sourceGridCell, endPoint)
  if (props.tab) {
    await keyDown('Tab')
  }
  if (midDragCallback != null) {
    midDragCallback(editor)
  }

  await mouseUpAtPoint(editor.renderedDOM.getByTestId(CanvasControlsContainerID), endPoint)

  return editor.renderedDOM.getByTestId(props.testId).style
}
