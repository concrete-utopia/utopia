import { safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { adjustNumberProperty, updateElementIndex, wildcardPatch } from '../commands/commands'
import { CanvasStrategy } from '../../../interactions_proposal'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'

function xCenter(target: ElementInstanceMetadata): number {
  return (target.globalFrame?.x ?? 0) + (target.globalFrame?.width ?? 0) / 2
}

function yCenter(target: ElementInstanceMetadata): number {
  return (target.globalFrame?.y ?? 0) + (target.globalFrame?.height ?? 0) / 2
}

export const flexReOrderStrategy: CanvasStrategy = {
  name: 'Re-order Flex Element',
  strategyGroups: new Set(),
  isApplicable: (canvasState, _interactionState) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedView = canvasState.selectedElements[0]
      const selectedMetadata = MetadataUtils.findElementByElementPath(
        canvasState.metadata,
        selectedView,
      )
      return selectedMetadata?.specialSizeMeasurements.parentLayoutSystem === 'flex'
    }
    return false
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState) => {
    return flexReOrderStrategy.isApplicable(canvasState, interactionState) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, _sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const target = MetadataUtils.findElementByElementPath(canvasState.metadata, targetedElement)
      if (target !== null) {
        const drag = interactionState.interactionData.drag
        const siblings = MetadataUtils.getSiblings(canvasState.metadata, targetedElement)
        const thisElementIndex = siblings.findIndex(
          (sibling) => getUtopiaID(sibling) === EP.toUid(targetedElement),
        )
        const flexDirection = target.specialSizeMeasurements.flexDirection ?? 'row' // TODO Check this is using the direction from the parent
        const isRowDirection = flexDirection.startsWith('row')
        const draggedRelevantDimensionCenter = isRowDirection
          ? xCenter(target) + drag.x
          : yCenter(target) + drag.y

        // Find the index of the first element that should be after the dragged element
        const firstElementAfterTargetIndex = siblings.findIndex((sibling, index) => {
          if (index === thisElementIndex) {
            return false // bail!
          } else {
            const siblingCenter = isRowDirection ? xCenter(sibling) : yCenter(sibling)
            return siblingCenter > draggedRelevantDimensionCenter
          }
        })

        const newIndex = Math.max(0, firstElementAfterTargetIndex)
        const indexChanged = newIndex !== thisElementIndex

        if (indexChanged) {
          return [
            updateElementIndex('permanent', targetedElement, newIndex),
            wildcardPatch('transient', {
              highlightedViews: {
                $set: [],
              },
              canvas: {
                domWalkerAdditionalElementsToUpdate: {
                  $set: siblings.map((metadata) => metadata.elementPath),
                },
              },
            }),
          ]
        }
      }
    }

    // Fallback for when the checks above are not satisfied.
    return []
  },
}
