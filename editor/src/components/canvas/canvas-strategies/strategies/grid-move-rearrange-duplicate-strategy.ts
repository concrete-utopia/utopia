import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import * as EP from '../../../../core/shared/element-path'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import { CSSCursor } from '../../../../uuiui-deps'
import { duplicateElement } from '../../commands/duplicate-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'

import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { CustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { runGridMoveRearrange } from './grid-helpers'
import { getGridTemplates, gridMoveStrategiesExtraCommands } from './grid-move-helpers'

export const gridMoveRearrangeDuplicateStrategy: CanvasStrategyFactory = (
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
    !interactionSession.interactionData.modifiers.alt
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

  return {
    id: 'rearrange-grid-move-duplicate-strategy',
    name: 'Grid Rearrange (Duplicate)',
    descriptiveLabel: 'Grid Rearrange (Duplicate)',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [controlsForGridPlaceholders(EP.parentPath(selectedElement))],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 3),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const oldUid = EP.toUid(selectedElement)

      let duplicatedElementNewUids = { ...customState.duplicatedElementNewUids }
      let newUid = duplicatedElementNewUids[oldUid]
      if (newUid == null) {
        newUid = 'dup-' + generateUidWithExistingComponents(canvasState.projectContents)
        duplicatedElementNewUids[oldUid] = newUid
      }

      const targetElement = EP.appendToPath(EP.parentPath(selectedElement), newUid)

      const selectedElementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        selectedElement,
      )
      if (selectedElementMetadata == null) {
        return emptyStrategyApplicationResult
      }

      const { parentGridCellGlobalFrames, parentContainerGridProperties } =
        selectedElementMetadata.specialSizeMeasurements

      const moveCommands =
        parentGridCellGlobalFrames != null
          ? runGridMoveRearrange(
              targetElement,
              selectedElement,
              canvasState.startingMetadata,
              interactionSession.interactionData,
              parentGridCellGlobalFrames,
              parentContainerGridProperties,
            )
          : []
      if (moveCommands.length === 0) {
        return emptyStrategyApplicationResult
      }

      const { midInteractionCommands, onCompleteCommands } = gridMoveStrategiesExtraCommands(
        parentGridPath,
        initialTemplates,
      )

      return strategyApplicationResult(
        [
          duplicateElement('always', selectedElement, newUid),
          ...moveCommands,
          ...midInteractionCommands,
          ...onCompleteCommands,
          updateSelectedViews('always', [targetElement]),
          updateHighlightedViews('always', [targetElement]),
          setCursorCommand(CSSCursor.Duplicate),
        ],
        [...selectedElements, targetElement],
        {
          duplicatedElementNewUids: duplicatedElementNewUids,
        },
      )
    },
  }
}
