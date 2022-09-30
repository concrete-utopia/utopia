import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { getOptionalDisplayPropCommands, isValidFlowReorderTarget } from './flow-reorder-helpers'
import { FlowReorderDragOutline } from '../controls/flow-reorder-indicators'
import { AllElementProps } from '../../editor/store/editor-state'
import { applyReorderCommon } from './reorder-utils'

function isFlowReorderConversionApplicable(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): boolean {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length === 1) {
    const target = selectedElements[0]
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, target)
    const siblings = MetadataUtils.getSiblings(metadata, target)
    if (
      siblings.length > 1 &&
      MetadataUtils.isPositionedByFlow(elementMetadata) &&
      isValidFlowReorderTarget(target, metadata)
    ) {
      return siblings.some((sibling) => MetadataUtils.isPositionedByFlow(sibling))
    } else {
      return false
    }
  } else {
    return false
  }
}

export const flowReorderStrategy: CanvasStrategy = {
  id: 'FLOW_REORDER',
  name: () => 'Reorder (Flow)',
  isApplicable: isFlowReorderConversionApplicable, // TODO this should check if the siblings are placed in the same row or same column
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
    {
      control: FlowReorderDragOutline,
      key: 'flow-reorder-drag-outline',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    return flowReorderStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 3
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    const reorderResult = applyReorderCommon(
      canvasState,
      interactionState,
      strategyState,
      isValidFlowReorderTarget,
    )

    const commands = [
      ...reorderResult.commands,
      ...getOptionalDisplayPropCommands(
        reorderResult.customStatePatch.lastReorderIdx,
        canvasState.interactionTarget,
        strategyState.startingMetadata,
      ),
    ]

    return strategyApplicationResult(commands, reorderResult.customStatePatch, reorderResult.status)
  },
}
