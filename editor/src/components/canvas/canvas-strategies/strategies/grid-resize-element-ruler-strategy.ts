import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridElementProperties } from '../../../../core/shared/element-template'
import { gridPositionValue } from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import { canvasRectangle, isInfinityRectangle } from '../../../../core/shared/math-utils'
import { gridContainerIdentifier, gridItemIdentifier } from '../../../editor/store/editor-state'
import { isCSSKeyword } from '../../../inspector/common/css-utils'
import { isFillOrStretchModeAppliedOnAnySide } from '../../../inspector/inspector-common'
import {
  controlsForGridPlaceholders,
  GridResizeControls,
} from '../../controls/grid-controls-for-strategies'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { getGridChildCellCoordBoundsFromCanvas } from './grid-cell-bounds'
import { getCommandsForGridItemPlacement } from './grid-helpers'

export const gridResizeElementRulerStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return null
  }
  const isElementInsideGrid = MetadataUtils.isGridItem(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (!isElementInsideGrid) {
    return null
  }

  const selectedElementBounds = MetadataUtils.getFrameInCanvasCoords(
    selectedElement,
    canvasState.startingMetadata,
  )
  if (selectedElementBounds == null || isInfinityRectangle(selectedElementBounds)) {
    return null
  }

  if (!isFillOrStretchModeAppliedOnAnySide(canvasState.startingMetadata, selectedElement)) {
    return null
  }

  if (interactionSession?.activeControl.type !== 'GRID_RESIZE_RULER_HANDLE') {
    return null
  }

  return {
    id: 'GRID-CELL-RESIZE-RULER-STRATEGY',
    name: 'Resize Grid Cell (ruler)',
    descriptiveLabel: 'Resize Grid Cell (ruler)',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridResizeControls,
        props: { target: gridContainerIdentifier(selectedElement) },
        key: `grid-resize-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
      controlsForGridPlaceholders(gridItemIdentifier(selectedElement)),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_RESIZE_RULER_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_RESIZE_RULER_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const allCellBounds =
        selectedElementMetadata.specialSizeMeasurements.parentGridCellGlobalFrames

      if (allCellBounds == null) {
        return emptyStrategyApplicationResult
      }

      const bounds = getGridChildCellCoordBoundsFromCanvas(selectedElementMetadata, allCellBounds)
      if (bounds == null) {
        return emptyStrategyApplicationResult
      }

      const columns = allCellBounds[0]
      const columnsWithAddition =
        // count one extra column so we correctly find the eastmost bound
        columns.concat(
          canvasRectangle({
            x: columns[columns.length - 1].x + columns[columns.length - 1].width,
            y: columns[columns.length - 1].y,
            width: 0,
            height: 0,
          }),
        )
      const closestHorizontal = getClosestCell(
        columnsWithAddition,
        'x',
        interactionSession.interactionData.drag,
        interactionSession.interactionData.dragStart,
      )

      const rows = allCellBounds.map((r) => r[0])
      const rowsWithAddition =
        // count one extra row so we correctly find the southmost bound
        rows.concat(
          canvasRectangle({
            x: rows[rows.length - 1].x,
            y: rows[rows.length - 1].y + rows[rows.length - 1].height,
            width: 0,
            height: 0,
          }),
        )
      const closestVertical = getClosestCell(
        rowsWithAddition,
        'y',
        interactionSession.interactionData.drag,
        interactionSession.interactionData.dragStart,
      )

      let gridProps: GridElementProperties = {
        ...selectedElementMetadata.specialSizeMeasurements.elementGridProperties,
      }
      if (closestHorizontal != null) {
        if (interactionSession.activeControl.edge === 'column-end') {
          gridProps.gridColumnEnd = gridPositionValue(
            Math.max(bounds.column + 1, closestHorizontal),
          )
          if (
            isCSSKeyword(gridProps.gridColumnStart) &&
            gridProps.gridColumnStart.value === 'auto'
          ) {
            gridProps.gridColumnStart = gridPositionValue(bounds.column)
          }
        } else if (interactionSession.activeControl.edge === 'column-start') {
          gridProps.gridColumnStart = gridPositionValue(
            Math.min(bounds.column + bounds.width - 1, closestHorizontal),
          )
          if (isCSSKeyword(gridProps.gridColumnEnd) && gridProps.gridColumnEnd.value === 'auto') {
            gridProps.gridColumnEnd = gridPositionValue(bounds.column + bounds.width)
          }
        }
      }
      if (closestVertical != null) {
        if (interactionSession.activeControl.edge === 'row-end') {
          gridProps.gridRowEnd = gridPositionValue(Math.max(bounds.row + 1, closestVertical))
          if (isCSSKeyword(gridProps.gridRowStart) && gridProps.gridRowStart.value === 'auto') {
            gridProps.gridRowStart = gridPositionValue(bounds.row)
          }
        } else if (interactionSession.activeControl.edge === 'row-start') {
          gridProps.gridRowStart = gridPositionValue(
            Math.min(bounds.row + bounds.height - 1, closestVertical),
          )
          if (isCSSKeyword(gridProps.gridRowEnd) && gridProps.gridRowEnd.value === 'auto') {
            gridProps.gridRowEnd = gridPositionValue(bounds.row + bounds.height)
          }
        }
      }

      const gridTemplate =
        selectedElementMetadata.specialSizeMeasurements.parentContainerGridProperties

      return strategyApplicationResult(
        getCommandsForGridItemPlacement(selectedElement, gridTemplate, gridProps),
        [EP.parentPath(selectedElement)],
      )
    },
  }
}

function getClosestCell(
  cells: CanvasRectangle[],
  axis: 'x' | 'y',
  drag: CanvasVector,
  dragStart: CanvasPoint,
): number | null {
  let closest: number | null = null
  let minDist = Infinity
  for (let i = 0; i < cells.length; i++) {
    const cell = cells[i]
    const position = drag[axis] + dragStart[axis]
    const dist = Math.abs(cell[axis] - position)
    if (dist < minDist) {
      minDist = dist
      closest = i + 1 // plus one because it's a grid pin, so it's 1-indexed
    }
  }
  return closest
}
