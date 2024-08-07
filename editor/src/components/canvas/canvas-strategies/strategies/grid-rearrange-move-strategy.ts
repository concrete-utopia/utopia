import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { GridControls } from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { CustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { runGridRearrangeMove } from './grid-helpers'

export const gridRearrangeMoveStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customState: CustomStrategyState,
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

  return {
    id: 'rearrange-grid-move-strategy',
    name: 'Rearrange Grid (Move)',
    descriptiveLabel: 'Rearrange Grid (Move)',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridControls,
        props: { targets: [EP.parentPath(selectedElement)] },
        key: `grid-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
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

      const targetElement = selectedElement

      const {
        commands: moveCommands,
        targetCell: targetGridCell,
        draggingFromCell,
        originalRootCell,
        targetRootCell,
      } = runGridRearrangeMove(
        targetElement,
        selectedElement,
        canvasState.startingMetadata,
        interactionSession.interactionData,
        canvasState.scale,
        canvasState.canvasOffset,
        customState.grid,
        false,
      )
      if (moveCommands.length === 0) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult(moveCommands, {
        grid: {
          targetCell: targetGridCell,
          draggingFromCell: draggingFromCell,
          originalRootCell: originalRootCell,
          currentRootCell: targetRootCell,
        },
      })
    },
  }
}
