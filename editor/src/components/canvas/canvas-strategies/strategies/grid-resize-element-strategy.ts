import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { offsetPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { create } from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { GridControls, GridResizeControls } from '../../controls/grid-controls'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { getGridCellUnderMouse } from './grid-helpers'

export const gridResizeElementStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customState,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const isElementInsideGrid = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      EP.parentPath(selectedElement),
    ),
  )
  if (!isElementInsideGrid) {
    return null
  }

  return {
    id: 'grid-cell-resize-strategy',
    name: 'Resize Grid Cell',
    descriptiveLabel: 'Resize Grid Cell',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridResizeControls,
        props: { target: selectedElement },
        key: `grid-resize-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
      {
        control: GridControls,
        props: {},
        key: `grid-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_RESIZE_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_RESIZE_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const mouseWindowPoint = canvasPointToWindowPoint(
        offsetPoint(
          interactionSession.interactionData.dragStart,
          interactionSession.interactionData.drag,
        ),
        canvasState.scale,
        canvasState.canvasOffset,
      )

      let targetCell = customState.targetGridCell
      const cellUnderMouse = getGridCellUnderMouse(mouseWindowPoint)
      if (cellUnderMouse != null) {
        targetCell = cellUnderMouse.coordinates
      }

      if (targetCell == null) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult(
        resizeGridCellCommands(selectedElement, {
          columnEnd: targetCell.column + 1,
          rowEnd: targetCell.row + 1,
        }),
        { targetGridCell: targetCell },
      )
    },
  }
}

function resizeGridCellCommands(
  elementPath: ElementPath,
  { columnEnd, rowEnd }: { columnEnd: number; rowEnd: number },
): CanvasCommand[] {
  return [
    setProperty('always', elementPath, create('style', 'gridColumnEnd'), columnEnd),
    setProperty('always', elementPath, create('style', 'gridRowEnd'), rowEnd),
  ]
}
