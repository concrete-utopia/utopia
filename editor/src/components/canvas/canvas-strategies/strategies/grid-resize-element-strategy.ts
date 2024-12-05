import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  GridElementProperties,
  GridPositionOrSpan,
} from '../../../../core/shared/element-template'
import {
  gridSpanNumeric,
  isGridPositionValue,
  isGridSpan,
} from '../../../../core/shared/element-template'
import {
  type CanvasRectangle,
  isInfinityRectangle,
  rectangleIntersection,
} from '../../../../core/shared/math-utils'
import { gridContainerIdentifier, gridItemIdentifier } from '../../../editor/store/editor-state'
import { cssKeyword } from '../../../inspector/common/css-utils'
import { isFillOrStretchModeAppliedOnAnySide } from '../../../inspector/inspector-common'
import {
  controlsForGridPlaceholders,
  gridEdgeToEdgePosition,
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
import { getCommandsForGridItemPlacement, isAutoGridPin } from './grid-helpers'
import { resizeBoundingBoxFromSide } from './resize-helpers'

export const gridResizeElementStrategy: CanvasStrategyFactory = (
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
        props: { target: gridContainerIdentifier(selectedElement) },
        key: `grid-resize-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
      controlsForGridPlaceholders(gridItemIdentifier(selectedElement)),
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

      const allCellBounds =
        selectedElementMetadata.specialSizeMeasurements.parentGridCellGlobalFrames

      if (allCellBounds == null) {
        return emptyStrategyApplicationResult
      }

      const resizeBoundingBox = resizeBoundingBoxFromSide(
        selectedElementBounds,
        interactionSession.interactionData.drag,
        gridEdgeToEdgePosition(interactionSession.activeControl.edge),
        'non-center-based',
        null,
      )

      const gridPropsNumeric = getNewGridPropsFromResizeBox(resizeBoundingBox, allCellBounds)

      if (gridPropsNumeric == null) {
        return emptyStrategyApplicationResult
      }

      const gridTemplate =
        selectedElementMetadata.specialSizeMeasurements.parentContainerGridProperties

      const elementGridPropertiesFromProps =
        selectedElementMetadata.specialSizeMeasurements.elementGridPropertiesFromProps

      const columnCount =
        gridPropsNumeric.gridColumnEnd.numericalPosition -
        gridPropsNumeric.gridColumnStart.numericalPosition
      const rowCount =
        gridPropsNumeric.gridRowEnd.numericalPosition -
        gridPropsNumeric.gridRowStart.numericalPosition

      const gridProps: GridElementProperties = {
        gridColumnStart: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridColumnStart,
          gridPropsNumeric.gridColumnStart,
          columnCount,
          'start',
          elementGridPropertiesFromProps.gridColumnEnd,
          gridPropsNumeric.gridColumnEnd,
          interactionSession.activeControl.edge,
        ),
        gridColumnEnd: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridColumnEnd,
          gridPropsNumeric.gridColumnEnd,
          columnCount,
          'end',
          elementGridPropertiesFromProps.gridColumnStart,
          gridPropsNumeric.gridColumnStart,
          interactionSession.activeControl.edge,
        ),
        gridRowStart: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridRowStart,
          gridPropsNumeric.gridRowStart,
          rowCount,
          'start',
          elementGridPropertiesFromProps.gridRowEnd,
          gridPropsNumeric.gridRowEnd,
          interactionSession.activeControl.edge,
        ),
        gridRowEnd: normalizeGridElementPositionAfterResize(
          elementGridPropertiesFromProps.gridRowEnd,
          gridPropsNumeric.gridRowEnd,
          rowCount,
          'end',
          elementGridPropertiesFromProps.gridRowStart,
          gridPropsNumeric.gridRowStart,
          interactionSession.activeControl.edge,
        ),
      }

      return strategyApplicationResult(
        getCommandsForGridItemPlacement(selectedElement, gridTemplate, gridProps),
        [EP.parentPath(selectedElement), selectedElement],
      )
    },
  }
}

function getNewGridPropsFromResizeBox(
  resizeBoundingBox: CanvasRectangle,
  allCellBounds: CanvasRectangle[][],
) {
  let newRowStart = Infinity
  let newRowEnd = -Infinity
  let newColumnStart = Infinity
  let newColumnEnd = -Infinity

  // those cells should be occupied by the element which has an intersection with the resize box
  for (let rowIdx = 0; rowIdx < allCellBounds.length; rowIdx++) {
    for (let colIdx = 0; colIdx < allCellBounds[rowIdx].length; colIdx++) {
      if (rectangleIntersection(resizeBoundingBox, allCellBounds[rowIdx][colIdx]) != null) {
        newRowStart = Math.min(newRowStart, rowIdx + 1)
        newColumnStart = Math.min(newColumnStart, colIdx + 1)
        newRowEnd = Math.max(newRowEnd, rowIdx + 2)
        newColumnEnd = Math.max(newColumnEnd, colIdx + 2)
      }
    }
  }

  if (
    !isFinite(newRowStart) ||
    !isFinite(newColumnStart) ||
    !isFinite(newRowEnd) ||
    !isFinite(newColumnEnd)
  ) {
    return null
  }

  return {
    gridRowStart: { numericalPosition: newRowStart },
    gridRowEnd: { numericalPosition: newRowEnd },
    gridColumnStart: { numericalPosition: newColumnStart },
    gridColumnEnd: { numericalPosition: newColumnEnd },
  }
}

/*
    After a resize happens and we know the numerical grid positioning of the new bounds,
    return a normalized version of the new position so that it respects any spans that
    may have been there before the resize, and/or default it to 'auto' when it would become redundant.
    If the positions match a flow configuration, give priority to span notation.
*/
export function normalizeGridElementPositionAfterResize(
  position: GridPositionOrSpan | null,
  resizedPosition: GridPositionOrSpan | null,
  size: number, // the number of cols/rows the cell occupies
  bound: 'start' | 'end',
  counterpart: GridPositionOrSpan | null,
  counterpartResizedPosition: GridPositionOrSpan | null,
  edge: GridResizeEdge,
): GridPositionOrSpan | null {
  function isFlowResizeOnBound(
    wantedBound: 'start' | 'end',
    flowStart: GridPositionOrSpan | null,
    flowEnd: GridPositionOrSpan | null,
  ): boolean {
    return (
      (edge === 'column-end' || edge === 'row-end') &&
      bound === wantedBound &&
      (isGridSpan(flowStart) || isAutoGridPin(flowStart) || flowStart == null) &&
      (isAutoGridPin(flowEnd) || flowEnd == null)
    )
  }

  const isFlowStart = isFlowResizeOnBound('start', position, counterpart)
  if (isFlowStart || isGridSpan(position)) {
    if (size === 1) {
      return cssKeyword('auto')
    }
    return gridSpanNumeric(size)
  }

  const isFlowEnd = isFlowResizeOnBound('end', counterpart, position)
  if (isFlowEnd) {
    return cssKeyword('auto')
  }

  if (
    isGridSpan(counterpart) &&
    isGridPositionValue(counterpartResizedPosition) &&
    counterpartResizedPosition.numericalPosition === 1 &&
    bound === 'end'
  ) {
    return cssKeyword('auto')
  }
  return resizedPosition
}
