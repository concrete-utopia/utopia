import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { switchToAbsolute } from '../commands/switch-to-absolute'
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
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return flowMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      ((interactionState.interactionData.drag?.x ?? 0) > 20 ||
        (interactionState.interactionData.drag?.y ?? 0) > 20)
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      return [switchToAbsolute('permanent', canvasState.selectedElements)]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
