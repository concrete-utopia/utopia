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
  isApplicable: (canvasState, _interactionState, metadata) => {
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
  fitness: (canvasState, interactionState, customStrategyState) => {
    return absoluteMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, customStrategyState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      return applyMoveCommon(
        canvasState,
        interactionState,
        getAdjustMoveCommands(canvasState, interactionState),
      )
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}
