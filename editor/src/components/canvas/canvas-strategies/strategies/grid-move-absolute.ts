import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  type ElementInstanceMetadata,
  type ElementInstanceMetadataMap,
  type GridContainerProperties,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  isInfinityRectangle,
  offsetPoint,
  pointDifference,
  zeroRectangle,
  zeroRectIfNullOrInfinity,
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
import type { GridCellGlobalFrames } from './grid-helpers'
import {
  getGlobalFrameOfGridCell,
  getOriginalElementGridConfiguration,
  getParentGridTemplatesFromChildMeasurements,
  gridMoveStrategiesExtraCommands,
} from './grid-helpers'
import { runGridChangeElementLocation } from './grid-change-element-location-strategy'
import { getTargetGridCellData } from '../../../inspector/grid-helpers'
import { gridItemIdentifier } from '../../../editor/store/editor-state'

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
      ...gridChildAbsoluteMoveCommands(
        selectedElementMetadata,
        zeroRectIfNullOrInfinity(
          selectedElementMetadata.specialSizeMeasurements.immediateParentBounds,
        ),
        interactionData,
      ),
    ]
  }

  function getContainingRect(): CanvasRectangle {
    if (selectedElementMetadata.specialSizeMeasurements.immediateParentProvidesLayout) {
      const gridCellGlobalFrame = getGlobalFrameOfGridCell(gridCellGlobalFrames, targetRootCell)
      return zeroRectIfNullOrInfinity(gridCellGlobalFrame)
    } else {
      const closestOffsetGlobalFrame = MetadataUtils.findElementByElementPath(
        jsxMetadata,
        selectedElementMetadata.specialSizeMeasurements.closestOffsetParentPath,
      )?.globalFrame
      return zeroRectIfNullOrInfinity(closestOffsetGlobalFrame)
    }
  }

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
    ...gridChildAbsoluteMoveCommands(selectedElementMetadata, getContainingRect(), interactionData),
  ]
}

function gridChildAbsoluteMoveCommands(
  element: ElementInstanceMetadata,
  containingRect: CanvasRectangle,
  dragInteractionData: DragInteractionData,
): CanvasCommand[] {
  if (
    element.globalFrame == null ||
    isInfinityRectangle(element.globalFrame) ||
    dragInteractionData.drag == null
  ) {
    return []
  }

  const offsetInTarget = pointDifference(containingRect, element.globalFrame)
  const dragOffset = offsetPoint(offsetInTarget, dragInteractionData.drag)

  return [
    deleteProperties('always', element.elementPath, [
      PP.create('style', 'top'),
      PP.create('style', 'left'),
      PP.create('style', 'right'),
      PP.create('style', 'bottom'),
    ]),
    setCssLengthProperty(
      'always',
      element.elementPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(dragOffset.y, null) },
      null,
    ),
    setCssLengthProperty(
      'always',
      element.elementPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(dragOffset.x, null) },
      null,
    ),
  ]
}
