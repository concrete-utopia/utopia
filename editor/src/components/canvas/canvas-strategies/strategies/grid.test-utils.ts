import { getRectCenter, localRectangle } from '../../../../core/shared/math-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'
import { GridCellTestId } from '../../controls/grid-controls-for-strategies'
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
import type { ElementPath } from 'utopia-shared/src/types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'

export async function runGridMoveTest(
  editor: EditorRenderResult,
  props: {
    scale: number
    gridPath: string
    testId: string
    targetCell?: GridCellCoordinates
    draggedCell?: GridCellCoordinates
    tab?: boolean
  },
  midDragCallback?: (editor: EditorRenderResult) => void,
) {
  const elementPathToDrag = EP.appendToPath(EP.fromString(props.gridPath), props.testId)

  await selectComponentsForTest(editor, [elementPathToDrag])

  if (props.scale !== 1) {
    await editor.dispatch([CanvasActions.zoom(props.scale)], true)
  }

  // trigger the grid controls so we know where to find the cells
  const { sourceGridCell, dragFrom, dragTo } = await getSafeGridMoveTestEndpoints(
    editor,
    elementPathToDrag,
    props.gridPath,
    {
      draggedCell: props.draggedCell,
      targetCell: props.targetCell,
    },
  )

  await mouseDownAtPoint(sourceGridCell, dragFrom)
  await mouseMoveToPoint(sourceGridCell, dragTo)
  if (props.tab) {
    await keyDown('Tab')
  }
  if (midDragCallback != null) {
    midDragCallback(editor)
  }

  await mouseUpAtPoint(editor.renderedDOM.getByTestId(CanvasControlsContainerID), dragTo)

  return editor.renderedDOM.getByTestId(props.testId).style
}

export async function getSafeGridMoveTestEndpoints(
  editor: EditorRenderResult,
  elementPathToDrag: ElementPath,
  gridPath: string,
  props?: {
    draggedCell?: GridCellCoordinates
    targetCell?: GridCellCoordinates
  },
) {
  const gridCellTestId = GridCellTestId(elementPathToDrag)
  const gridCellBaseElement = editor.renderedDOM.getByTestId(gridCellTestId)
  const gridCellBaseElementBox = gridCellBaseElement.getBoundingClientRect()
  const testPoint = getRectCenter(localRectangle(gridCellBaseElementBox))

  await mouseDownAtPoint(gridCellBaseElement, testPoint)

  const sourceGridCell = editor.renderedDOM.getByTestId(
    props?.draggedCell == null
      ? gridCellTestId
      : gridCellTargetId(
          EP.fromString(gridPath),
          props?.draggedCell.row,
          props?.draggedCell.column,
        ),
  )

  const targetGridCell = editor.renderedDOM.getByTestId(
    gridCellTargetId(
      EP.fromString(gridPath),
      props?.targetCell?.row ?? 2,
      props?.targetCell?.column ?? 3,
    ),
  )
  const sourceRect = sourceGridCell.getBoundingClientRect()
  const dragFrom = { x: sourceRect.x + 5, y: sourceRect.y + 5 }

  const targetRect = targetGridCell.getBoundingClientRect()
  const dragTo = { x: targetRect.x + 5, y: targetRect.y + 5 }

  await mouseUpAtPoint(editor.renderedDOM.getByTestId(CanvasControlsContainerID), testPoint)

  return { sourceGridCell, dragFrom, dragTo }
}
