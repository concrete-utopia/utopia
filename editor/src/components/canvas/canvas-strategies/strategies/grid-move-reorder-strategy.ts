import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  GridContainerProperties,
} from '../../../../core/shared/element-template'
import { isInfinityRectangle, offsetPoint } from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { absolute } from '../../../../utils/utils'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
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
import { getClosestGridCellToPoint, gridCellCoordinates } from './grid-cell-bounds'
import type { GridCellGlobalFrames } from './grid-helpers'
import {
  findOriginalGrid,
  getGridElementPinState,
  getGridPositionIndex,
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
  isFlowGridChild,
} from './grid-helpers'

export const gridMoveReorderStrategy: CanvasStrategyFactory = (
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
    interactionSession.interactionData.modifiers.alt ||
    interactionSession.interactionData.modifiers.cmd // disable reorder when reparenting, for now (TODO)
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
  if (selectedElementMetadata == null) {
    return null
  }
  if (MetadataUtils.isPositionAbsolute(selectedElementMetadata)) {
    return null
  }

  const initialTemplates = getParentGridTemplatesFromChildMeasurements(
    selectedElementMetadata.specialSizeMeasurements,
  )
  if (initialTemplates == null) {
    return null
  }

  const parentGridPath = EP.parentPath(selectedElement)
  const gridFrame = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    parentGridPath,
  )?.globalFrame
  if (gridFrame == null || isInfinityRectangle(gridFrame)) {
    return null
  }

  const elementGridPropertiesFromProps =
    selectedElementMetadata.specialSizeMeasurements.elementGridPropertiesFromProps

  const pinnedState = getGridElementPinState(elementGridPropertiesFromProps ?? null)
  const fitnessModifier = pinnedState !== 'pinned' ? 1 : -1

  return {
    id: 'reorder-grid-move-strategy',
    name: 'Grid Reorder',
    descriptiveLabel: 'Grid Reorder',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [controlsForGridPlaceholders(parentGridPath, 'visible-only-while-active')],
    fitness: onlyFitWhenDraggingThisControl(
      interactionSession,
      'GRID_CELL_HANDLE',
      2 + fitnessModifier,
    ),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const { commands, elementsToRerender } = getCommandsAndPatchForGridReorder(
        canvasState,
        interactionSession.interactionData,
        selectedElement,
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

function getCommandsAndPatchForGridReorder(
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

  const gridPath = findOriginalGrid(canvasState.startingMetadata, EP.parentPath(selectedElement)) // TODO don't use EP.parentPath
  if (gridPath == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const { parentGridCellGlobalFrames, parentContainerGridProperties } =
    selectedElementMetadata.specialSizeMeasurements
  if (parentGridCellGlobalFrames == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridMoveReorder(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

function runGridMoveReorder(
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
  gridPath: ElementPath,
  gridCellGlobalFrames: GridCellGlobalFrames,
  gridTemplate: GridContainerProperties,
): CanvasCommand[] {
  if (interactionData.drag == null) {
    return []
  }

  const mouseCellPosInOriginalElement = getOriginalElementGridConfiguration(
    gridCellGlobalFrames,
    interactionData,
    selectedElementMetadata,
  )?.mouseCellPosInOriginalElement
  if (mouseCellPosInOriginalElement == null) {
    return []
  }

  const mousePos = offsetPoint(interactionData.dragStart, interactionData.drag)
  const targetCellData = getClosestGridCellToPoint(gridCellGlobalFrames, mousePos)
  const targetCellCoords = targetCellData?.gridCellCoordinates
  if (targetCellCoords == null) {
    return []
  }

  const row = Math.max(targetCellCoords.row - mouseCellPosInOriginalElement.row, 1)
  const column = Math.max(targetCellCoords.column - mouseCellPosInOriginalElement.column, 1)

  const gridTemplateColumns =
    gridTemplate.gridTemplateColumns?.type === 'DIMENSIONS'
      ? gridTemplate.gridTemplateColumns.dimensions.length
      : 1

  const gridChildren = MetadataUtils.getChildrenUnordered(jsxMetadata, gridPath)
  const gridFlowChildrenCount = gridChildren.filter(isFlowGridChild).length

  // The "pure" index in the grid children for the cell under mouse
  const possiblyReorderIndex = getGridPositionIndex({
    row: targetCellCoords.row,
    column: targetCellCoords.column,
    gridTemplateColumns: gridTemplateColumns,
  })

  const canReorderToIndex = possiblyReorderIndex < gridFlowChildrenCount

  const updateGridControlsCommand = showGridControls(
    'mid-interaction',
    gridPath,
    targetCellData?.gridCellCoordinates ?? null,
    canReorderToIndex ? gridCellCoordinates(row, column) : null,
  )

  return [
    reorderElement('always', selectedElementMetadata.elementPath, absolute(possiblyReorderIndex)),
    deleteProperties('always', selectedElementMetadata.elementPath, [
      PP.create('style', 'gridColumn'),
      PP.create('style', 'gridRow'),
      PP.create('style', 'gridColumnStart'),
      PP.create('style', 'gridColumnEnd'),
      PP.create('style', 'gridRowStart'),
      PP.create('style', 'gridRowEnd'),
    ]),
    updateGridControlsCommand,
  ]
}
