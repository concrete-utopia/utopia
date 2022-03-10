import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { JSXElement } from '../../../core/shared/element-template'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { CanvasStrategy } from './canvas-strategy-types'

export const absoluteResizeStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_RESIZE',
  name: 'Absolute Resize',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      })
    } else {
      return false
    }
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteResizeStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      return [
        wildcardPatch('transient', {
          highlightedViews: {
            $set: [],
          },
        }),
      ]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
