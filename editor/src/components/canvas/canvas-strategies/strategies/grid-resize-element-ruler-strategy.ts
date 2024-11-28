import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridElementProperties } from '../../../../core/shared/element-template'
import { gridPositionValue } from '../../../../core/shared/element-template'
import { canvasRectangle, isInfinityRectangle } from '../../../../core/shared/math-utils'
import { gridContainerIdentifier, gridItemIdentifier } from '../../../editor/store/editor-state'
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
      let closestHorizontal: number | null = null
      let minDistHoriz = Infinity
      const columnsWithAddition = columns.concat(
        canvasRectangle({
          x: columns[columns.length - 1].x + columns[columns.length - 1].width,
          y: columns[columns.length - 1].y,
          width: 0,
          height: 0,
        }),
      )
      for (let i = 0; i < columnsWithAddition.length; i++) {
        const column = columnsWithAddition[i]
        const dist = Math.abs(
          column.x -
            (interactionSession.interactionData.drag.x +
              interactionSession.interactionData.dragStart.x),
        )
        if (dist < minDistHoriz) {
          minDistHoriz = dist
          closestHorizontal = i + 1
        }
      }

      const rows = allCellBounds.map((r) => r[0])
      let closestVertical: number | null = null
      let minDistVert = Infinity
      const rowsWithAddition = rows.concat(
        canvasRectangle({
          x: rows[rows.length - 1].x,
          y: rows[rows.length - 1].y + rows[rows.length - 1].height,
          width: 0,
          height: 0,
        }),
      )
      for (let i = 0; i < rowsWithAddition.length; i++) {
        const row = rowsWithAddition[i]
        const dist = Math.abs(
          row.y -
            (interactionSession.interactionData.drag.y +
              interactionSession.interactionData.dragStart.y),
        )
        if (dist < minDistVert) {
          minDistVert = dist
          closestVertical = i + 1
        }
      }

      let gridProps: GridElementProperties = {
        ...selectedElementMetadata.specialSizeMeasurements.elementGridProperties,
      }
      if (closestHorizontal != null) {
        if (interactionSession.activeControl.edge === 'column-end') {
          gridProps.gridColumnEnd = gridPositionValue(closestHorizontal)
        } else if (interactionSession.activeControl.edge === 'column-start') {
          gridProps.gridColumnStart = gridPositionValue(closestHorizontal)
        }
      }
      if (closestVertical != null) {
        if (interactionSession.activeControl.edge === 'row-end') {
          gridProps.gridRowEnd = gridPositionValue(closestVertical)
        } else if (interactionSession.activeControl.edge === 'row-start') {
          gridProps.gridRowStart = gridPositionValue(closestVertical)
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
