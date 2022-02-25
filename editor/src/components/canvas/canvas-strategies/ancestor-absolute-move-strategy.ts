import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../core/shared/array-utils'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { CanvasStrategy } from './canvas-strategy-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { adjustNumberProperty, setProperty, wildcardPatch } from '../commands/commands'

export const ancestorAbsoluteMoveStrategy: CanvasStrategy = {
  name: 'Ancestor Absolute Move',
  strategyGroups: new Set(),
  isApplicable: (canvasState, interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1 && interactionState != null) {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )

      if (elementMetadata?.specialSizeMeasurements.position === 'static') {
        const ancestorWithLayout = MetadataUtils.findContainingBlock(
          metadata,
          canvasState.selectedElements[0],
        )
        return ancestorWithLayout != null
      } else {
        return false
      }
    }
    return false
  },
  controlsToRender: [], // parent padding control
  fitness: (canvasState, interactionState, sessionState) => {
    return ancestorAbsoluteMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) && interactionState.interactionData.type === 'DRAG'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const ancestorWithLayout = MetadataUtils.findContainingBlock(
        sessionState.startingMetadata,
        targetedElement,
      )
      const ancestorMetadata = MetadataUtils.findElementByElementPath(
        sessionState.startingMetadata,
        ancestorWithLayout,
      )

      if (ancestorWithLayout != null) {
        return [
          adjustNumberProperty(
            'permanent',
            ancestorWithLayout,
            stylePropPathMappingFn('left', ['style']),
            interactionState.interactionData.drag.x,
            true,
          ),
          adjustNumberProperty(
            'permanent',
            ancestorWithLayout,
            stylePropPathMappingFn('top', ['style']),
            interactionState.interactionData.drag.y,
            true,
          ),
          wildcardPatch('transient', {
            highlightedViews: {
              $set: [],
            },
          }),
        ]
      }
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
