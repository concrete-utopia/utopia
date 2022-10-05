import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { honoursPropsPosition } from './absolute-utils'
import {
  CanvasStrategy,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from './canvas-strategy-types'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  getDragTargets,
} from './shared-move-strategies-helpers'

export const absoluteMoveStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_MOVE',
  name: () => 'Move',
  isApplicable: (canvasState, interactionSession, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length > 0) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          honoursPropsPosition(canvasState, element)
        )
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionSession, customStrategyState) => {
    return absoluteMoveStrategy.isApplicable(
      canvasState,
      interactionSession,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionSession, customStrategyState) => {
    if (
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.interactionData.drag != null
    ) {
      return applyMoveCommon(
        canvasState,
        interactionSession,
        getAdjustMoveCommands(canvasState, interactionSession),
      )
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}
