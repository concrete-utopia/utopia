import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  type ElementInstanceMetadata,
  type ElementInstanceMetadataMap,
  type GridContainerProperties,
} from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
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
import type { GridCellGlobalFrames } from './grid-helpers'
import {
  getGridElementPinState,
  getGridPositionIndex,
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
  isFlowGridChild,
} from './grid-helpers'
import { getTargetGridCellData } from '../../../inspector/grid-helpers'
import { gridItemIdentifier } from '../../../editor/store/editor-state'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'

export const gridReorderStrategy: CanvasStrategyFactory = (
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
  if (!MetadataUtils.isGridItem(canvasState.startingMetadata, selectedElement)) {
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
    controlsToRender: [
      controlsForGridPlaceholders(gridItemIdentifier(selectedElement), 'visible-only-while-active'),
    ],
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

  const { parentGridCellGlobalFrames, parentContainerGridProperties } =
    selectedElementMetadata.specialSizeMeasurements
  if (parentGridCellGlobalFrames == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridReorder(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

function runGridReorder(
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
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

  const targetGridCellData = getTargetGridCellData(
    interactionData,
    gridCellGlobalFrames,
    mouseCellPosInOriginalElement,
  )
  if (targetGridCellData == null) {
    return []
  }
  const { targetCellCoords, targetRootCell } = targetGridCellData

  const gridTemplateColumns =
    gridTemplate.gridTemplateColumns?.type === 'DIMENSIONS'
      ? gridTemplate.gridTemplateColumns.dimensions.length
      : 1

  const gridChildren = MetadataUtils.getSiblingsUnordered(
    jsxMetadata,
    selectedElementMetadata.elementPath,
  )
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
    gridItemIdentifier(selectedElementMetadata.elementPath),
    targetCellCoords,
    canReorderToIndex ? targetRootCell : null,
  )

  const gridConfig = getOriginalElementGridConfiguration(
    gridCellGlobalFrames,
    interactionData,
    selectedElementMetadata,
  )

  const width = gridConfig?.originalCellBounds.width ?? 1
  const height = gridConfig?.originalCellBounds.height ?? 1

  const propsToUpdate: PropertyToUpdate[] = [
    propertyToDelete(PP.create('style', 'gridColumnStart')),
    propertyToDelete(PP.create('style', 'gridColumnEnd')),
    propertyToDelete(PP.create('style', 'gridRowStart')),
    propertyToDelete(PP.create('style', 'gridRowEnd')),
    ...(width > 1
      ? [propertyToSet(PP.create('style', 'gridColumn'), `span ${width}`)]
      : [propertyToDelete(PP.create('style', 'gridColumn'))]),
    ...(height > 1
      ? [propertyToSet(PP.create('style', 'gridRow'), `span ${height}`)]
      : [propertyToDelete(PP.create('style', 'gridRow'))]),
  ]

  return [
    reorderElement('always', selectedElementMetadata.elementPath, absolute(possiblyReorderIndex)),
    updateBulkProperties('always', selectedElementMetadata.elementPath, propsToUpdate),
    updateGridControlsCommand,
  ]
}
