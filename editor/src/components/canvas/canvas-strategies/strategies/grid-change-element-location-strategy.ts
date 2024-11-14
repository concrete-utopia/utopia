import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  GridContainerProperties,
  GridElementProperties,
  GridPositionOrSpan,
} from '../../../../core/shared/element-template'
import { gridPositionValue, isGridSpan } from '../../../../core/shared/element-template'
import { absolute } from '../../../../utils/utils'
import { gridItemIdentifier } from '../../../editor/store/editor-state'
import { cssKeyword } from '../../../inspector/common/css-utils'
import { getTargetGridCellData } from '../../../inspector/grid-helpers'
import type { CanvasCommand } from '../../commands/commands'
import { reorderElement } from '../../commands/reorder-element-command'
import { showGridControls } from '../../commands/show-grid-controls-command'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { DragInteractionData, InteractionSession } from '../interaction-state'
import type { GridCellGlobalFrames, SortableGridElementProperties } from './grid-helpers'
import {
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
  isAutoGridPin,
  setGridPropsCommands,
  sortElementsByGridPosition,
} from './grid-helpers'

export const gridChangeElementLocationStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length !== 1 ||
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null ||
    interactionSession.activeControl.type !== 'GRID_CELL_HANDLE' ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return null
  }

  const selectedElement = selectedElements[0]
  if (!MetadataUtils.isGridItem(canvasState.startingMetadata, selectedElement)) {
    return null
  }

  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (
    selectedElementMetadata == null ||
    !MetadataUtils.targetRegisteredStyleControlsOrHonoursStyleProps(
      canvasState.projectContents,
      selectedElementMetadata,
      canvasState.propertyControlsInfo,
      'layout',
      ['gridRow', 'gridColumn', 'gridRowStart', 'gridColumnStart', 'gridRowEnd', 'gridColumnEnd'],
      'every',
    )
  ) {
    return null
  }

  const initialTemplates = getParentGridTemplatesFromChildMeasurements(
    selectedElementMetadata.specialSizeMeasurements,
  )
  if (initialTemplates == null) {
    return null
  }

  if (MetadataUtils.isPositionAbsolute(selectedElementMetadata)) {
    return null
  }

  return {
    id: 'grid-change-element-location-strategy',
    name: 'Change Location',
    descriptiveLabel: 'Change Location',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      controlsForGridPlaceholders(gridItemIdentifier(selectedElement), 'visible-only-while-active'),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 2),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const { commands, elementsToRerender } = getCommandsAndPatchForGridChangeElementLocation(
        canvasState,
        interactionSession.interactionData,
        selectedElement,
      )
      if (commands.length === 0) {
        return emptyStrategyApplicationResult
      }

      const { midInteractionCommands, onCompleteCommands } = gridMoveStrategiesExtraCommands(
        EP.parentPath(selectedElement), // TODO: don't use EP.parentPath
        initialTemplates,
      )

      return strategyApplicationResult(
        [...midInteractionCommands, ...onCompleteCommands, ...commands],
        elementsToRerender,
      )
    },
  }
}

function getCommandsAndPatchForGridChangeElementLocation(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  selectedElement: ElementPath,
): {
  commands: CanvasCommand[]
  elementsToRerender: ElementPath[]
} {
  if (interactionData.drag == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const { parentGridCellGlobalFrames, parentContainerGridProperties } =
    selectedElementMetadata.specialSizeMeasurements
  if (parentGridCellGlobalFrames == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridChangeElementLocation(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
    null,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

export function runGridChangeElementLocation(
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
  gridCellGlobalFrames: GridCellGlobalFrames,
  gridTemplate: GridContainerProperties,
  newPathAfterReparent: ElementPath | null,
): CanvasCommand[] {
  if (interactionData.drag == null) {
    return []
  }

  const isReparent = newPathAfterReparent != null
  const pathForCommands = isReparent ? newPathAfterReparent : selectedElementMetadata.elementPath // when reparenting, we want to use the new path for commands

  const gridConfig = isReparent
    ? {
        originalCellBounds: { width: 1, height: 1 }, // when reparenting, we just put it in a single cell
        mouseCellPosInOriginalElement: { row: 0, column: 0 },
      }
    : getOriginalElementGridConfiguration(
        gridCellGlobalFrames,
        interactionData,
        selectedElementMetadata,
      )
  if (gridConfig == null) {
    return []
  }
  const { mouseCellPosInOriginalElement, originalCellBounds } = gridConfig

  const targetGridCellData = getTargetGridCellData(
    interactionData,
    gridCellGlobalFrames,
    mouseCellPosInOriginalElement,
  )
  if (targetGridCellData == null) {
    return []
  }
  const { targetCellCoords, targetRootCell } = targetGridCellData

  const elementGridProperties =
    selectedElementMetadata.specialSizeMeasurements.elementGridProperties

  function getUpdatedPins(
    start: GridPositionOrSpan | null,
    end: GridPositionOrSpan | null,
    axis: 'row' | 'column',
    dimension: number,
  ): {
    start: GridPositionOrSpan
    end: GridPositionOrSpan
  } {
    let result: {
      start: GridPositionOrSpan
      end: GridPositionOrSpan
    } = {
      start: cssKeyword('auto'),
      end: cssKeyword('auto'),
    }

    const isSpanning = isGridSpan(start) || isGridSpan(end)
    if (isSpanning) {
      if (isGridSpan(start)) {
        const isEndGridSpanArea = isGridSpan(end) || start.type === 'SPAN_AREA'
        if (!isEndGridSpanArea) {
          if (isAutoGridPin(end ?? cssKeyword('auto'))) {
            result.start = start
            result.end = gridPositionValue(start.value + targetRootCell[axis])
          } else {
            result.start = start
            result.end = gridPositionValue(start.value + targetRootCell[axis])
          }
        }
      } else if (isGridSpan(end)) {
        result.start = gridPositionValue(targetRootCell[axis])
        result.end = end
      }
    } else {
      result.start = gridPositionValue(targetRootCell[axis])
      const shouldSetEndPosition = end != null && !isAutoGridPin(end)
      if (shouldSetEndPosition) {
        result.end =
          dimension === 1 ? cssKeyword('auto') : gridPositionValue(targetRootCell[axis] + dimension)
      }
    }

    return result
  }

  const columnBounds = getUpdatedPins(
    elementGridProperties.gridColumnStart,
    elementGridProperties.gridColumnEnd,
    'column',
    originalCellBounds.width,
  )

  const rowBounds = getUpdatedPins(
    elementGridProperties.gridRowStart,
    elementGridProperties.gridRowEnd,
    'row',
    originalCellBounds.height,
  )

  const gridProps: GridElementProperties = {
    gridColumnStart: columnBounds.start,
    gridColumnEnd: columnBounds.end,
    gridRowStart: rowBounds.start,
    gridRowEnd: rowBounds.end,
  }

  const gridCellMoveCommands = setGridPropsCommands(pathForCommands, gridTemplate, gridProps)

  // The siblings of the grid element being moved
  const siblings = MetadataUtils.getSiblingsUnordered(
    jsxMetadata,
    newPathAfterReparent ?? selectedElementMetadata.elementPath,
  )
    .filter((s) => !EP.pathsEqual(s.elementPath, selectedElementMetadata.elementPath))
    .map(
      (s, index): SortableGridElementProperties => ({
        ...s.specialSizeMeasurements.elementGridProperties,
        index: index,
        path: s.elementPath,
      }),
    )

  // Sort the siblings and the cell under mouse ascending based on their grid coordinates, so that
  // the indexes grow left-right, top-bottom.
  const templateColumnsCount =
    gridTemplate.gridTemplateColumns?.type === 'DIMENSIONS'
      ? gridTemplate.gridTemplateColumns.dimensions.length
      : 1
  const cellsSortedByPosition = siblings
    .concat({
      ...{
        gridColumnStart: gridPositionValue(targetCellCoords.column),
        gridColumnEnd: gridPositionValue(targetCellCoords.column),
        gridRowStart: gridPositionValue(targetCellCoords.row),
        gridRowEnd: gridPositionValue(targetCellCoords.row),
      },
      path: selectedElementMetadata.elementPath,
      index: siblings.length + 1,
    })
    .sort(sortElementsByGridPosition(templateColumnsCount))

  const indexInSortedCellsForChangeLocation = cellsSortedByPosition.findIndex((s) =>
    EP.pathsEqual(selectedElementMetadata.elementPath, s.path),
  )

  const updateGridControlsCommand = showGridControls(
    'mid-interaction',
    gridItemIdentifier(selectedElementMetadata.elementPath),
    targetCellCoords,
    targetRootCell,
  )

  return [
    ...gridCellMoveCommands,
    reorderElement(
      'always',
      pathForCommands,
      absolute(Math.max(indexInSortedCellsForChangeLocation, 0)),
    ),
    updateGridControlsCommand,
  ]
}
