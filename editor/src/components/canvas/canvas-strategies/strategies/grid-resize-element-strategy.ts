import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridElementProperties } from '../../../../core/shared/element-template'
import { offsetPoint } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
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
import { getGridCellUnderMouse, setGridProps } from './grid-helpers'

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

      let gridProps: GridElementProperties = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        selectedElement,
      )?.specialSizeMeasurements.elementGridProperties ?? {
        gridColumnEnd: { numericalPosition: 0 },
        gridColumnStart: { numericalPosition: 0 },
        gridRowEnd: { numericalPosition: 0 },
        gridRowStart: { numericalPosition: 0 },
      }

      switch (interactionSession.activeControl.edge) {
        case 'column-start':
          gridProps = {
            ...gridProps,
            gridColumnStart: { numericalPosition: targetCell.column },
          }
          break
        case 'column-end':
          gridProps = {
            ...gridProps,
            gridColumnEnd: { numericalPosition: targetCell.column + 1 },
          }
          break
        case 'row-end':
          gridProps = {
            ...gridProps,
            gridRowEnd: { numericalPosition: targetCell.row + 1 },
          }
          break
        case 'row-start':
          gridProps = {
            ...gridProps,
            gridRowStart: { numericalPosition: targetCell.row },
          }
          break
        default:
          assertNever(interactionSession.activeControl.edge)
      }

      return strategyApplicationResult(setGridProps(selectedElement, gridProps), {
        targetGridCell: targetCell,
      })
    },
  }
}
