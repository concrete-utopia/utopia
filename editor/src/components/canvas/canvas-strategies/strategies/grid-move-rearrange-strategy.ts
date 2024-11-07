import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  GridContainerProperties,
} from '../../../../core/shared/element-template'
import { gridPositionValue } from '../../../../core/shared/element-template'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import { absolute } from '../../../../utils/utils'
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
  findOriginalGrid,
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
  setGridPropsCommands,
  sortElementsByGridPosition,
} from './grid-helpers'
import { getTargetGridCellData } from '../../../inspector/grid-helpers'

export const gridMoveRearrangeStrategy: CanvasStrategyFactory = (
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
  if (!MetadataUtils.isGridCell(canvasState.startingMetadata, selectedElement)) {
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
      'some',
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

  const parentGridPath = findOriginalGrid(
    canvasState.startingMetadata,
    EP.parentPath(selectedElement),
  ) // TODO don't use EP.parentPath
  if (parentGridPath == null) {
    return null
  }

  const gridFrame = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    parentGridPath,
  )?.globalFrame
  if (gridFrame == null || isInfinityRectangle(gridFrame)) {
    return null
  }

  if (MetadataUtils.isPositionAbsolute(selectedElementMetadata)) {
    return null
  }

  return {
    id: 'rearrange-grid-move-strategy',
    name: 'Grid rearrange',
    descriptiveLabel: 'Grid rearrange',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [controlsForGridPlaceholders(parentGridPath, 'visible-only-while-active')],
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

      const { commands, elementsToRerender } = getCommandsAndPatchForGridRearrange(
        canvasState,
        interactionSession.interactionData,
        selectedElement,
        parentGridPath,
      )
      if (commands.length === 0) {
        return emptyStrategyApplicationResult
      }

      const { midInteractionCommands, onCompleteCommands } = gridMoveStrategiesExtraCommands(
        parentGridPath,
        initialTemplates,
      )
      return strategyApplicationResult(
        [...midInteractionCommands, ...onCompleteCommands, ...commands],
        elementsToRerender,
      )
    },
  }
}

function getCommandsAndPatchForGridRearrange(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  selectedElement: ElementPath,
  gridPath: ElementPath,
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

  const commands = runGridMoveRearrange(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
    null,
  )

  return {
    commands: commands,
    elementsToRerender: [gridPath, selectedElement],
  }
}

export function runGridMoveRearrange(
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
  gridPath: ElementPath,
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

  const gridCellMoveCommands = setGridPropsCommands(pathForCommands, gridTemplate, {
    gridColumnStart: gridPositionValue(targetRootCell.column),
    gridColumnEnd: gridPositionValue(targetRootCell.column + originalCellBounds.height),
    gridRowStart: gridPositionValue(targetRootCell.row),
    gridRowEnd: gridPositionValue(targetRootCell.row + originalCellBounds.width),
  })

  // The siblings of the grid element being moved
  const siblings = MetadataUtils.getChildrenUnordered(jsxMetadata, gridPath)
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

  const indexInSortedCellsForRearrange = cellsSortedByPosition.findIndex((s) =>
    EP.pathsEqual(selectedElementMetadata.elementPath, s.path),
  )

  const updateGridControlsCommand = showGridControls(
    'mid-interaction',
    gridPath,
    targetCellCoords,
    targetRootCell,
  )

  return [
    ...gridCellMoveCommands,
    reorderElement(
      'always',
      pathForCommands,
      absolute(Math.max(indexInSortedCellsForRearrange, 0)),
    ),
    updateGridControlsCommand,
  ]
}
