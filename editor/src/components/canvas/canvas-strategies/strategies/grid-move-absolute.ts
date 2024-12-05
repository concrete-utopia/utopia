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
  isNotNullFiniteRectangle,
  nullIfInfinity,
  offsetPoint,
  offsetRect,
  rectangleContainsRectangleInclusive,
  rectContainsPoint,
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
import type { NewGridElementProps } from './grid-change-element-location-strategy'
import {
  getNewGridElementProps,
  runGridChangeElementLocation,
} from './grid-change-element-location-strategy'
import { getTargetGridCellData } from '../../../inspector/grid-helpers'
import { gridItemIdentifier } from '../../../editor/store/editor-state'
import { getMoveCommandsForDrag } from './shared-move-strategies-helpers'
import { toFirst } from '../../../../core/shared/optics/optic-utilities'
import { eitherRight, fromTypeGuard } from '../../../../core/shared/optics/optic-creators'
import { defaultEither } from '../../../../core/shared/either'
import { forceNotNull } from '../../../..//core/shared/optional-utils'
import { windowToCanvasCoordinates } from '../../dom-lookup'

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
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

export function getNewGridElementPropsCheckingOriginalGrid(
  originalGridMetadata: ElementInstanceMetadata,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
  gridCellGlobalFrames: GridCellGlobalFrames,
  newPathAfterReparent: ElementPath | null,
): NewGridElementProps | null {
  if (interactionData.drag == null) {
    return null
  }

  // Identify the containing block position and size.
  const originalContainingBlockRectangle = getGridRelativeContainingBlock(
    originalGridMetadata,
    selectedElementMetadata,
    selectedElementMetadata.specialSizeMeasurements.elementGridProperties,
  )

  // Capture the original position of the grid child.
  const originalCanvasFrame = selectedElementMetadata.globalFrame
  if (!isNotNullFiniteRectangle(originalCanvasFrame)) {
    return null
  }

  // Capture the parent bounds of the grid child.
  const coordinateSystemBounds =
    selectedElementMetadata.specialSizeMeasurements.immediateParentBounds
  if (coordinateSystemBounds == null) {
    return null
  }

  // Identify if the new position of the grid child is wholly inside the containing block's
  // global frame.
  const containingBlockGlobalFrame = offsetRect(
    canvasRectangle(originalContainingBlockRectangle),
    coordinateSystemBounds,
  )
  const gridChildNewGlobalFrame = offsetRect(originalCanvasFrame, interactionData.drag)
  const insideOriginalContainingBlock = rectangleContainsRectangleInclusive(
    containingBlockGlobalFrame,
    gridChildNewGlobalFrame,
  )

  // If the element is inside the containing block,
  // then don't attempt to move it.
  if (insideOriginalContainingBlock) {
    return null
  }

  return getNewGridElementProps(
    interactionData,
    selectedElementMetadata,
    gridCellGlobalFrames,
    newPathAfterReparent,
  )
}

function runGridMoveAbsolute(
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  selectedElementMetadata: ElementInstanceMetadata,
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
  const element = defaultEither(
    null,
    toFirst(
      eitherRight<string, JSXElementChild>().compose(fromTypeGuard(isJSXElement)),
      selectedElementMetadata.element,
    ),
  )
  if (element == null) {
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
        element,
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

  const coordinateSystemBounds =
    selectedElementMetadata.specialSizeMeasurements.immediateParentBounds ?? zeroCanvasRect

  // The element may be moving to a different grid position, which is then used
  // to calculate the potentially new containing block.
  const newGridElementProps = getNewGridElementPropsCheckingOriginalGrid(
    originalGrid,
    interactionData,
    selectedElementMetadata,
    gridCellGlobalFrames,
    null,
  )

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
    ...(newGridElementProps == null
      ? []
      : runGridChangeElementLocation(
          jsxMetadata,
          interactionData,
          selectedElementMetadata,
          gridCellGlobalFrames,
          gridTemplate,
          null,
        )),
    ...getMoveCommandsForDrag(
      containingRect,
      element,
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
