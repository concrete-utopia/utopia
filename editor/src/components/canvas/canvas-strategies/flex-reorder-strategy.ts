import { safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { adjustNumberProperty, updateElementIndex, wildcardPatch } from '../commands/commands'
import { CanvasStrategy } from '../../../interactions_proposal'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { isRight } from '../../../core/shared/either'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { ElementPath } from '../../../core/shared/project-file-types'

function xCenter(target: ElementInstanceMetadata): number {
  return (target.globalFrame?.x ?? 0) + (target.globalFrame?.width ?? 0) / 2
}

function yCenter(target: ElementInstanceMetadata): number {
  return (target.globalFrame?.y ?? 0) + (target.globalFrame?.height ?? 0) / 2
}

export const flexReOrderStrategy: CanvasStrategy = {
  name: 'Re-order Flex Element',
  strategyGroups: new Set(),
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedView = canvasState.selectedElements[0]
      const selectedMetadata = MetadataUtils.findElementByElementPath(metadata, selectedView)
      return selectedMetadata?.specialSizeMeasurements.parentLayoutSystem === 'flex'
    }
    return false
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return flexReOrderStrategy.isApplicable(
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
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const targetAtStart = MetadataUtils.findElementByElementPath(
        sessionState.startingMetadata,
        targetedElement,
      )
      const parent = MetadataUtils.getParent(sessionState.startingMetadata, targetedElement)
      if (targetAtStart !== null && parent !== null) {
        const parentPath: ElementPath = parent.elementPath
        const siblingPaths = withUnderlyingTarget(
          parentPath,
          canvasState.projectContents,
          {},
          canvasState.openFile,
          [],
          (success, parentElement, _underlyingTarget, _underlyingFilePath) => {
            const siblingUIDs = parentElement.children.map(getUtopiaID)
            return siblingUIDs.map((uid) => EP.appendToPath(parentPath, uid))
          },
        )

        const drag = interactionState.interactionData.drag

        const siblings = MetadataUtils.findElementsByElementPath(
          sessionState.startingMetadata,
          siblingPaths,
        )
        // FIXME There still appears to be a caching issue somewhere here

        const thisElementIndex = siblings.findIndex(
          (sibling) => getUtopiaID(sibling) === EP.toUid(targetedElement),
        )
        const flexDirection = MetadataUtils.getFlexDirection(parent)
        const isRowDirection = flexDirection.startsWith('row')
        const draggedRelevantDimensionCenter = isRowDirection
          ? xCenter(targetAtStart) + drag.x
          : yCenter(targetAtStart) + drag.y

        // Find the index of the first element that should be after the dragged element
        const firstElementAfterTargetIndex = siblings.findIndex((sibling, index) => {
          const siblingCenter = isRowDirection ? xCenter(sibling) : yCenter(sibling)
          return siblingCenter > draggedRelevantDimensionCenter
        })

        const movedBack = firstElementAfterTargetIndex <= thisElementIndex
        const movedToEnd = firstElementAfterTargetIndex < 0

        const newIndex = movedToEnd
          ? siblings.length - 1
          : movedBack
          ? firstElementAfterTargetIndex
          : firstElementAfterTargetIndex - 1

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
