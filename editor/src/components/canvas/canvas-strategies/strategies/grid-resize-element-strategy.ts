import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridElementProperties, GridPosition } from '../../../../core/shared/element-template'
import { offsetPoint } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { isCSSKeyword } from '../../../inspector/common/css-utils'
import { isFixedHugFillModeApplied } from '../../../inspector/inspector-common'
import { controlsForGridPlaceholders, GridResizeControls } from '../../controls/grid-controls'
import { canvasRectangleToWindowRectangle } from '../../dom-lookup'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { getGridCellUnderMouseFromMetadata } from './grid-cell-bounds'
import { setGridPropsCommands } from './grid-helpers'

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
  const isElementInsideGrid = MetadataUtils.isGridCell(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (!isElementInsideGrid) {
    return null
  }

  const isFillOrStretchContainer =
    isFixedHugFillModeApplied(canvasState.startingMetadata, selectedElement, 'fill') ||
    isFixedHugFillModeApplied(canvasState.startingMetadata, selectedElement, 'stretch')
  if (!isFillOrStretchContainer) {
    return null
  }

  const parentGridPath = EP.parentPath(selectedElement)

  return {
    id: 'GRID-CELL-RESIZE-STRATEGY',
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
      controlsForGridPlaceholders(parentGridPath),
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

      const container = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        EP.parentPath(selectedElement),
      )
      if (container == null) {
        return emptyStrategyApplicationResult
      }

      const mouseCanvasPoint = offsetPoint(
        interactionSession.interactionData.dragStart,
        interactionSession.interactionData.drag,
      )

      const cellUnderMouse = getGridCellUnderMouseFromMetadata(container, mouseCanvasPoint)
      const targetCell = cellUnderMouse == null ? customState.grid.targetCellData : cellUnderMouse

      if (targetCell == null) {
        return emptyStrategyApplicationResult
      }

      const gridTemplate = container.specialSizeMeasurements.containerGridProperties

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
            gridColumnStart: { numericalPosition: targetCell.gridCellCoordinates.column },
          }
          break
        case 'column-end':
          gridProps = {
            ...gridProps,
            gridColumnEnd: { numericalPosition: targetCell.gridCellCoordinates.column + 1 },
          }
          break
        case 'row-end':
          gridProps = {
            ...gridProps,
            gridRowEnd: { numericalPosition: targetCell.gridCellCoordinates.row + 1 },
          }
          break
        case 'row-start':
          gridProps = {
            ...gridProps,
            gridRowStart: { numericalPosition: targetCell.gridCellCoordinates.row },
          }
          break
        default:
          assertNever(interactionSession.activeControl.edge)
      }

      return strategyApplicationResult(
        setGridPropsCommands(selectedElement, gridTemplate, gridPropsWithDragOver(gridProps)),
        [parentGridPath],
        {
          grid: { ...customState.grid, targetCellData: targetCell },
        },
      )
    },
  }
}

function orderedGridPositions({
  start,
  end,
}: {
  start: GridPosition | null
  end: GridPosition | null
}): {
  start: GridPosition | null
  end: GridPosition | null
} {
  if (
    start == null ||
    isCSSKeyword(start) ||
    start.numericalPosition == null ||
    end == null ||
    isCSSKeyword(end) ||
    end.numericalPosition == null
  ) {
    return { start, end }
  }

  return start.numericalPosition < end.numericalPosition
    ? { start, end }
    : {
        start: { numericalPosition: end.numericalPosition - 1 },
        end: { numericalPosition: start.numericalPosition + 1 },
      }
}

function gridPropsWithDragOver(props: GridElementProperties): GridElementProperties {
  const { start: gridColumnStart, end: gridColumnEnd } = orderedGridPositions({
    start: props.gridColumnStart,
    end: props.gridColumnEnd,
  })
  const { start: gridRowStart, end: gridRowEnd } = orderedGridPositions({
    start: props.gridRowStart,
    end: props.gridRowEnd,
  })

  return { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }
}
