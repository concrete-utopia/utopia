import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  GridContainerProperties,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasVector,
  isInfinityRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { cssNumber } from '../../../inspector/common/css-utils'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
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
  getGlobalFrameOfGridCell,
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
} from './grid-helpers'
import { runGridMoveRearrange } from './grid-move-rearrange-strategy'

export const gridMoveAbsoluteStrategy: CanvasStrategyFactory = (
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
  if (selectedElementMetadata == null) {
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

  if (!MetadataUtils.isPositionAbsolute(selectedElementMetadata)) {
    return null
  }

  return {
    id: 'absolute-grid-move-strategy',
    name: 'Grid move (Abs)',
    descriptiveLabel: 'Grid move (Abs)',
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

      const { commands, elementsToRerender } = getCommandsAndPatchForGridAbsoluteMove(
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

function getCommandsAndPatchForGridAbsoluteMove(
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

  const commands = runGridMoveAbsolute(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [gridPath, selectedElement],
  }
}

function runGridMoveAbsolute(
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

  const gridConfig = getOriginalElementGridConfiguration(
    gridCellGlobalFrames,
    interactionData,
    selectedElementMetadata,
  )
  if (gridConfig == null) {
    return []
  }
  const { mouseCellPosInOriginalElement } = gridConfig

  const mousePos = offsetPoint(interactionData.dragStart, interactionData.drag)
  const targetCellData = getClosestGridCellToPoint(gridCellGlobalFrames, mousePos)
  const targetCellCoords = targetCellData?.gridCellCoordinates
  if (targetCellCoords == null) {
    return []
  }

  const row = Math.max(targetCellCoords.row - mouseCellPosInOriginalElement.row, 1)
  const column = Math.max(targetCellCoords.column - mouseCellPosInOriginalElement.column, 1)

  const targetRootCell = gridCellCoordinates(row, column)

  // if moving an absolutely-positioned child which does not have pinning
  // props, do not set them at all.
  if (MetadataUtils.hasNoGridCellPositioning(selectedElementMetadata.specialSizeMeasurements)) {
    const updateGridControlsCommand = showGridControls(
      'mid-interaction',
      gridPath,
      targetCellCoords,
      targetRootCell,
    )

    const absoluteMoveCommands = gridChildAbsoluteMoveCommands(
      MetadataUtils.findElementByElementPath(jsxMetadata, selectedElementMetadata.elementPath),
      MetadataUtils.getFrameOrZeroRectInCanvasCoords(gridPath, jsxMetadata),
      interactionData,
    )
    return [...absoluteMoveCommands, updateGridControlsCommand]
  }

  // otherwise, return a rearrange move + absolute adjustment
  const canvasRect = getGlobalFrameOfGridCell(gridCellGlobalFrames, targetRootCell)
  if (canvasRect == null) {
    return []
  }
  const absoluteMoveCommands = gridChildAbsoluteMoveCommands(
    MetadataUtils.findElementByElementPath(jsxMetadata, selectedElementMetadata.elementPath),
    canvasRect,
    interactionData,
  )
  const rearrangeCommands = runGridMoveRearrange(
    jsxMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    gridCellGlobalFrames,
    gridTemplate,
    null,
  )
  return [...rearrangeCommands, ...absoluteMoveCommands]
}

function gridChildAbsoluteMoveCommands(
  targetMetadata: ElementInstanceMetadata | null,
  containingRect: CanvasRectangle,
  dragInteractionData: DragInteractionData,
): CanvasCommand[] {
  if (
    targetMetadata == null ||
    targetMetadata.globalFrame == null ||
    isInfinityRectangle(targetMetadata.globalFrame) ||
    !MetadataUtils.isPositionAbsolute(targetMetadata)
  ) {
    return []
  }

  const offsetInTarget = canvasPoint({
    x: dragInteractionData.originalDragStart.x - targetMetadata.globalFrame.x,
    y: dragInteractionData.originalDragStart.y - targetMetadata.globalFrame.y,
  })

  const dragOffset = offsetPoint(
    dragInteractionData.originalDragStart,
    dragInteractionData.drag ?? canvasPoint({ x: 0, y: 0 }),
  )

  const offset = canvasVector({
    x: dragOffset.x - containingRect.x - offsetInTarget.x,
    y: dragOffset.y - containingRect.y - offsetInTarget.y,
  })

  return [
    deleteProperties('always', targetMetadata.elementPath, [
      PP.create('style', 'top'),
      PP.create('style', 'left'),
      PP.create('style', 'right'),
      PP.create('style', 'bottom'),
    ]),
    setCssLengthProperty(
      'always',
      targetMetadata.elementPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(offset.y, null) },
      null,
    ),
    setCssLengthProperty(
      'always',
      targetMetadata.elementPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(offset.x, null) },
      null,
    ),
  ]
}
