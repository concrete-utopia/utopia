import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  GridElementProperties,
  GridPositionOrSpan,
} from '../../../../core/shared/element-template'
import { gridPositionValue, isGridPositionValue } from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import { canvasRectangle, isInfinityRectangle } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
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
import type { GridResizeEdge, InteractionSession } from '../interaction-state'
import type { GridCellCoordBounds } from './grid-cell-bounds'
import { getGridChildCellCoordBoundsFromCanvas } from './grid-cell-bounds'
import { getCommandsForGridItemPlacement } from './grid-helpers'
import { normalizeGridElementPositionAfterResize } from './grid-resize-element-strategy'

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

      if (!isFillOrStretchModeAppliedOnAnySide(canvasState.startingMetadata, selectedElement)) {
        // TODO this should be removed to support resizing cells that are not stretching
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

      if (allCellBounds.length < 1) {
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
      const closestHorizontal = getClosestCellAlongAxis(
        columnsWithAddition,
        'x',
        interactionSession.interactionData.drag,
        interactionSession.interactionData.dragStart,
      )

      const rows = mapDropNulls((r) => {
        if (r.length < 1) {
          return null
        }
        return r[0]
      }, allCellBounds)
      if (rows.length < 1) {
        return emptyStrategyApplicationResult
      }
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
      const closestVertical = getClosestCellAlongAxis(
        rowsWithAddition,
        'y',
        interactionSession.interactionData.drag,
        interactionSession.interactionData.dragStart,
      )

      const elementGridPropertiesFromProps =
        selectedElementMetadata.specialSizeMeasurements.elementGridPropertiesFromProps

      const resizedProps = getResizedElementProperties(
        interactionSession.activeControl.edge,
        selectedElementMetadata.specialSizeMeasurements.elementGridProperties,
        bounds,
        closestHorizontal,
        closestVertical,
      )

      const columnCount = getCellsCount(resizedProps.gridColumnStart, resizedProps.gridColumnEnd)
      const rowCount = getCellsCount(resizedProps.gridRowStart, resizedProps.gridRowEnd)

      const normalizedGridProps: GridElementProperties = {
        gridColumnStart: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridColumnStart,
          resizedProps.gridColumnStart,
          columnCount ?? bounds.width,
          'start',
          elementGridPropertiesFromProps.gridColumnEnd,
          resizedProps.gridColumnEnd,
          interactionSession.activeControl.edge,
        ),
        gridColumnEnd: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridColumnEnd,
          resizedProps.gridColumnEnd,
          columnCount ?? bounds.width,
          'end',
          elementGridPropertiesFromProps.gridColumnStart,
          resizedProps.gridColumnStart,
          interactionSession.activeControl.edge,
        ),
        gridRowStart: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridRowStart,
          resizedProps.gridRowStart,
          rowCount ?? bounds.height,
          'start',
          elementGridPropertiesFromProps.gridRowEnd,
          resizedProps.gridRowEnd,
          interactionSession.activeControl.edge,
        ),
        gridRowEnd: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridRowEnd,
          resizedProps.gridRowEnd,
          rowCount ?? bounds.height,
          'end',
          elementGridPropertiesFromProps.gridRowStart,
          resizedProps.gridRowStart,
          interactionSession.activeControl.edge,
        ),
      }

      const gridTemplate =
        selectedElementMetadata.specialSizeMeasurements.parentContainerGridProperties

      return strategyApplicationResult(
        getCommandsForGridItemPlacement(selectedElement, gridTemplate, normalizedGridProps),
        [EP.parentPath(selectedElement), selectedElement],
      )
    },
  }
}

function getClosestCellAlongAxis(
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

function getResizedElementProperties(
  edge: GridResizeEdge,
  initial: GridElementProperties,
  bounds: GridCellCoordBounds,
  closestHorizontal: number | null,
  closestVertical: number | null,
): GridElementProperties {
  function maybeAuto(
    position: GridPositionOrSpan | null,
    valueIfAuto: number,
  ): GridPositionOrSpan | null {
    if (isCSSKeyword(position) && position.value === 'auto') {
      return gridPositionValue(valueIfAuto)
    }
    return position
  }

  switch (edge) {
    case 'column-end':
      return {
        ...initial,
        gridColumnEnd:
          closestHorizontal != null
            ? gridPositionValue(Math.max(bounds.column + 1, closestHorizontal))
            : initial.gridColumnEnd,
        gridColumnStart: maybeAuto(initial.gridColumnStart, bounds.column),
      }
    case 'column-start':
      return {
        ...initial,
        gridColumnStart:
          closestHorizontal != null
            ? gridPositionValue(Math.min(bounds.column + bounds.width - 1, closestHorizontal))
            : initial.gridColumnStart,
        gridColumnEnd: maybeAuto(initial.gridColumnEnd, bounds.column + bounds.width),
      }
    case 'row-end':
      return {
        ...initial,
        gridRowEnd:
          closestVertical != null
            ? gridPositionValue(Math.max(bounds.row + 1, closestVertical))
            : initial.gridRowEnd,
        gridRowStart: maybeAuto(initial.gridRowStart, bounds.row),
      }
    case 'row-start':
      return {
        ...initial,
        gridRowStart:
          closestVertical != null
            ? gridPositionValue(Math.min(bounds.row + bounds.height - 1, closestVertical))
            : initial.gridRowStart,
        gridRowEnd: maybeAuto(initial.gridRowEnd, bounds.row + bounds.height),
      }
    default:
      assertNever(edge)
  }
}

function getCellsCount(
  start: GridPositionOrSpan | null,
  end: GridPositionOrSpan | null,
): number | null {
  if (isGridPositionValue(start) && start.numericalPosition != null) {
    if (isGridPositionValue(end) && end.numericalPosition != null) {
      return end.numericalPosition - start.numericalPosition
    }
    return start.numericalPosition
  }
  return null
}
