import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { ControlWithProps, InteractionCanvasState } from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { DragInteractionData, InteractionSession } from '../interaction-state'
import {
  getCommandsAndPatchForGridRearrange,
  getGridTemplates,
  gridMoveStrategiesExtraCommands,
} from './grid-move-helpers'

export const gridMoveRearrangeStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
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

  const parentGridPath = EP.parentPath(selectedElement)
  const gridFrame = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    parentGridPath,
  )?.globalFrame
  if (gridFrame == null || isInfinityRectangle(gridFrame)) {
    return null
  }

  const initialTemplates = getGridTemplates(canvasState.startingMetadata, parentGridPath)
  if (initialTemplates == null) {
    return null
  }

  const strategyToApply = getStrategyToApply(
    parentGridPath,
    canvasState.startingMetadata,
    selectedElement,
  )
  if (strategyToApply == null) {
    return null
  }

  return {
    id: 'rearrange-grid-move-strategy',
    name: strategyToApply.name,
    descriptiveLabel: strategyToApply.name,
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: strategyToApply.controlsToRender,
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

type StrategyToApply = {
  controlsToRender: ControlWithProps<any>[]
  name: string
}

function getStrategyToApply(
  parentGridPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  cell: ElementPath,
): StrategyToApply {
  const element = MetadataUtils.findElementByElementPath(jsxMetadata, cell)

  const name =
    MetadataUtils.isPositionAbsolute(element) &&
    !MetadataUtils.isGridCellWithPositioning(jsxMetadata, cell)
      ? 'Grid Move (Abs)'
      : 'Grid Rearrange'

  return {
    name: name,
    controlsToRender: [controlsForGridPlaceholders(parentGridPath, 'visible-only-while-active')],
  }
}
