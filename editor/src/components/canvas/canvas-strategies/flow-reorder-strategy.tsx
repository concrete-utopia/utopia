import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { areAllSiblingsInOneDimension, isValidFlowReorderTarget } from './flow-reorder-helpers'
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
    return (
      MetadataUtils.isPositionedByFlow(elementMetadata) &&
      isValidFlowReorderTarget(target, metadata) &&
      areAllSiblingsInOneDimension(target, metadata)
    )
  } else {
    return false
  }
}

export const flowReorderStrategy: CanvasStrategy = {
  id: 'FLOW_REORDER',
  name: () => 'Reorder (Flow)',
  isApplicable: isFlowReorderConversionApplicable,
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
  fitness: (canvasState, interactionSession, customStrategyState) => {
    return flowReorderStrategy.isApplicable(
      canvasState,
      interactionSession,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
      ? 3
      : 0
  },
  apply: (canvasState, interactionSession, customStrategyState) => {
    return applyReorderCommon(
      canvasState,
      interactionSession,
      customStrategyState,
      isValidFlowReorderTarget,
    )
  },
}
