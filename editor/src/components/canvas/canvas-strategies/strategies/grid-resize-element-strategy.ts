import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridElementProperties, GridPosition } from '../../../../core/shared/element-template'
import {
  type CanvasRectangle,
  isInfinityRectangle,
  rectangleIntersection,
} from '../../../../core/shared/math-utils'
import { isCSSKeyword } from '../../../inspector/common/css-utils'
import { isFixedHugFillModeApplied } from '../../../inspector/inspector-common'
import {
  controlsForGridPlaceholders,
  gridEdgeToEdgePosition,
  GridResizeControls,
} from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { getMetadataWithGridCellBounds, setGridPropsCommands } from './grid-helpers'
import { resizeBoundingBoxFromSide } from './resize-helpers'

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

  const selectedElementBounds = MetadataUtils.getFrameInCanvasCoords(
    selectedElement,
    canvasState.startingMetadata,
  )
  if (selectedElementBounds == null || isInfinityRectangle(selectedElementBounds)) {
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

      const { metadata: container, foundIn } = getMetadataWithGridCellBounds(
        EP.parentPath(selectedElement),
        canvasState.startingMetadata,
        interactionSession.latestMetadata,
        customState,
      )

      if (container == null) {
        return emptyStrategyApplicationResult
      }

      const allCellBounds = container.specialSizeMeasurements.gridCellGlobalFrames

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

      const selectedElementGridProps = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        selectedElement,
      )?.specialSizeMeasurements.elementGridProperties

      const gridProps = getNewGridPropsFromResizeBox(
        resizeBoundingBox,
        selectedElementGridProps ?? null,
        allCellBounds,
      )

      if (gridProps == null) {
        return emptyStrategyApplicationResult
      }

      const gridTemplate = container.specialSizeMeasurements.containerGridProperties

      const customStatePatch =
        foundIn === 'latestMetadata'
          ? {
              ...customState,
              grid: {
                ...customState.grid,
                metadataCacheForGrids: {
                  ...customState.grid.metadataCacheForGrids,
                  [EP.toString(container.elementPath)]: container,
                },
              },
            }
          : {}
      return strategyApplicationResult(
        setGridPropsCommands(selectedElement, gridTemplate, gridProps),
        [parentGridPath],
        customStatePatch,
      )
    },
  }
}

function getNewGridPropsFromResizeBox(
  resizeBoundingBox: CanvasRectangle,
  gridProps: GridElementProperties | null,
  allCellBounds: CanvasRectangle[][],
) {
  let newRowStart = Infinity
  let newRowEnd = -Infinity
  let newColumnStart = Infinity
  let newColumnEnd = -Infinity

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
