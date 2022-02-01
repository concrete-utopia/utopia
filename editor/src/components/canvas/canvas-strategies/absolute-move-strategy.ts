import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasStrategy } from '../../../interactions_proposal'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { adjustNumberProperty, setProperty, wildcardPatch } from '../commands/commands'

export const absoluteMoveStrategy: CanvasStrategy = {
  name: 'Absolute Move',
  isApplicable: (canvasState, _interactionState) => {
    if (canvasState.selectedElements.length === 1) {
      const metadata = MetadataUtils.findElementByElementPath(
        canvasState.metadata,
        canvasState.selectedElements[0],
      )

      return metadata?.specialSizeMeasurements.position === 'absolute'
    } else {
      return false
    }
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState) => {
    return absoluteMoveStrategy.isApplicable(canvasState, interactionState) &&
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
        ),
        adjustNumberProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn('top', ['style']),
          drag.y,
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
