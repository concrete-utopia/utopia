import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { switchToAbsolute } from '../commands/switch-to-absolute'
import { FlowMoveControls } from '../controls/flow-move-controls'
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
    { control: FlowMoveControls, key: 'flow-move-control', show: 'visible-only-while-active' },
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
      if (interactionState.globalTime - interactionState.lastInteractionTime > 2000) {
        return [switchToAbsolute('permanent', canvasState.selectedElements)]
      }
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
