import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { JSXElementChild } from '../../../../core/shared/element-template'
import {
  isJSXElement,
  type ElementInstanceMetadata,
  type ElementInstanceMetadataMap,
  type GridContainerProperties,
} from '../../../../core/shared/element-template'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  canvasRectangleToLocalRectangle,
  nullIfInfinity,
  offsetRect,
  windowPoint,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
  zeroSize,
} from '../../../../core/shared/math-utils'
import type { CanvasCommand } from '../../commands/commands'
import { showGridControls } from '../../commands/show-grid-controls-command'
import {
  controlsForGridPlaceholders,
  GridElementChildContainingBlockKey,
} from '../../controls/grid-controls-for-strategies'
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
  findOriginalGrid,
  getGridRelativeContainingBlock,
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
} from './grid-helpers'
import {
  getNewGridElementProps,
  runGridChangeElementLocation,
} from './grid-change-element-location-strategy'
import { getTargetGridCellData } from '../../../inspector/grid-helpers'
import { gridItemIdentifier } from '../../../editor/store/editor-state'
import { getMoveCommandsForDrag } from './shared-move-strategies-helpers'
import type { StyleInfo } from '../../canvas-types'

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
  const initialTemplates = getParentGridTemplatesFromChildMeasurements(
    selectedElementMetadata.specialSizeMeasurements,
  )
  if (initialTemplates == null) {
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

      const { commands, elementsToRerender } = getCommandsAndPatchForGridAbsoluteMove(
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

function getCommandsAndPatchForGridAbsoluteMove(
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

  const commands = runGridMoveAbsolute(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    canvasState.styleInfoReader(selectedElement),
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

function runGridMoveAbsolute(
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
  styleInfo: StyleInfo | null,
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

  const targetGridCellData = getTargetGridCellData(
    interactionData,
    gridCellGlobalFrames,
    mouseCellPosInOriginalElement,
  )
  if (targetGridCellData == null) {
    return []
  }
  const { targetCellCoords, targetRootCell } = targetGridCellData
  if (styleInfo == null) {
    return []
  }

  const globalFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(selectedElementMetadata.elementPath, jsxMetadata),
  )

  if (globalFrame == null) {
    return []
  }

  const localFrame = nullIfInfinity(
    MetadataUtils.getLocalFrame(
      selectedElementMetadata.elementPath,
      jsxMetadata,
      EP.parentPath(selectedElementMetadata.elementPath),
    ),
  )

  // if moving an absolutely-positioned child which does not have pinning
  // props, do not set them at all.
  if (MetadataUtils.hasNoGridItemPositioning(selectedElementMetadata.specialSizeMeasurements)) {
    return [
      showGridControls(
        'mid-interaction',
        gridItemIdentifier(selectedElementMetadata.elementPath),
        targetCellCoords,
        targetRootCell,
      ),
      ...getMoveCommandsForDrag(
        zeroRectIfNullOrInfinity(
          selectedElementMetadata.specialSizeMeasurements.immediateParentBounds,
        ),
        styleInfo,
        selectedElementMetadata.elementPath,
        selectedElementMetadata.elementPath,
        interactionData.drag,
        globalFrame,
        localFrame,
        null,
        false,
      ).commands,
    ]
  }

  // The element may be moving to a different grid position, which is then used
  // to calculate the potentially new containing block.
  const newGridElementProps = getNewGridElementProps(
    interactionData,
    selectedElementMetadata,
    gridCellGlobalFrames,
    null,
  )

  const coordinateSystemBounds =
    selectedElementMetadata.specialSizeMeasurements.immediateParentBounds ?? zeroCanvasRect

  // Get the metadata of the original grid.
  const originalGridPath = findOriginalGrid(
    jsxMetadata,
    EP.parentPath(selectedElementMetadata.elementPath),
  )
  if (originalGridPath == null) {
    return []
  }
  const originalGrid = MetadataUtils.findElementByElementPath(jsxMetadata, originalGridPath)
  if (originalGrid == null) {
    return []
  }

  // Get the containing block of the grid child.
  const containingBlockRectangle = getGridRelativeContainingBlock(
    originalGrid,
    selectedElementMetadata,
    newGridElementProps?.gridElementProperties ??
      selectedElementMetadata.specialSizeMeasurements.elementGridProperties,
  )

  // Get the appropriately shifted and typed local frame value to use.
  const containingRect = offsetRect(
    canvasRectangle(containingBlockRectangle),
    coordinateSystemBounds,
  )
  const adjustedLocalFrame = canvasRectangleToLocalRectangle(globalFrame, containingRect)

  // otherwise, return a change location + absolute adjustment
  return [
    ...runGridChangeElementLocation(
      jsxMetadata,
      interactionData,
      selectedElementMetadata,
      gridCellGlobalFrames,
      gridTemplate,
      null,
    ),
    ...getMoveCommandsForDrag(
      containingRect,
      styleInfo,
      selectedElementMetadata.elementPath,
      selectedElementMetadata.elementPath,
      interactionData.drag,
      globalFrame,
      adjustedLocalFrame,
      null,
      false,
    ).commands,
  ]
}
