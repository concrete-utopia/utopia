import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { adjustNumberProperty } from '../commands/adjust-number-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import * as EP from '../../../core/shared/element-path'
import { CanvasStrategy } from './canvas-strategy-types'

export const absoluteMoveStrategy: CanvasStrategy = {
  name: 'Absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )

      return elementMetadata?.specialSizeMeasurements.position === 'absolute'
    } else {
      return false
    }
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteMoveStrategy.isApplicable(
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
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const drag = interactionState.interactionData.drag
      return canvasState.selectedElements.flatMap((selectedElement) => [
        adjustNumberProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn('left', ['style']),
          drag.x,
          true,
        ),
        adjustNumberProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn('top', ['style']),
          drag.y,
          true,
        ),
        wildcardPatch('transient', {
          highlightedViews: {
            $set: [],
          },
        }),
      ])
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
