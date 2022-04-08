import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { switchToAbsolute } from '../commands/switch-to-absolute'
import {
  AnimationTimer,
  ConversionHighlightOutline,
  ConversionTargetButton,
  FlowGhostOutline,
  FlowMoveControlTimer,
  FlowMoveControlTooltip,
} from '../controls/flow-move-controls'
import { CanvasStrategy } from './canvas-strategy-types'

export const flowMoveStrategy: CanvasStrategy = {
  id: 'FLOW_MOVE',
  name: 'Flow Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'static'
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    {
      control: FlowMoveControlTooltip,
      key: 'flow-move-control-tooltip',
      show: 'visible-only-while-active',
    },
    {
      control: FlowMoveControlTimer,
      key: 'flow-move-control-timer',
      show: 'visible-only-while-active',
    },
    {
      control: FlowGhostOutline,
      key: 'flow-ghost-outline',
      show: 'visible-only-while-active',
    },
    {
      control: ConversionTargetButton,
      key: 'conversion-target-button',
      show: 'visible-only-while-active',
    },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return flowMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (interactionState.interactionData.type === 'DRAG') {
      if (interactionState.globalTime - interactionState.lastInteractionTime > AnimationTimer) {
        if (interactionState.interactionData.modifiers.alt) {
          return [switchToAbsolute('permanent', canvasState.selectedElements, 'all')]
        } else {
          return [switchToAbsolute('permanent', canvasState.selectedElements, 'selected')]
        }
      }
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
