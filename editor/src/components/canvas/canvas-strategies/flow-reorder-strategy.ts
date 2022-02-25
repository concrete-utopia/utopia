import { safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { adjustNumberProperty, updateElementIndex, wildcardPatch } from '../commands/commands'
import { CanvasStrategy } from './canvas-strategy-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { isRight } from '../../../core/shared/either'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { ElementPath } from '../../../core/shared/project-file-types'

function yCenter(target: ElementInstanceMetadata): number {
  return (target.globalFrame?.y ?? 0) + (target.globalFrame?.height ?? 0) / 2
}

export const flowReOrderStrategy: CanvasStrategy = {
  name: 'Re-order Flow Element',
  strategyGroups: new Set(),
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedView = canvasState.selectedElements[0]
      const selectedMetadata = MetadataUtils.findElementByElementPath(metadata, selectedView)
      return selectedMetadata?.specialSizeMeasurements.parentLayoutSystem === 'flow'
    }
    return false
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return flowReOrderStrategy.isApplicable(
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
      if (targetAtStart != null && parent != null) {
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

        const draggedCenter = yCenter(targetAtStart) + drag.y

        const siblings = MetadataUtils.findElementsByElementPath(
          sessionState.startingMetadata,
          siblingPaths,
        )
        // FIXME There still appears to be a caching issue somewhere here

        const thisElementIndex = siblings.findIndex(
          (sibling) => getUtopiaID(sibling) === EP.toUid(targetedElement),
        )

        let indexToMoveTo: number | null = null
        siblings.forEach((sibling, index) => {
          const siblingCenter = yCenter(sibling)
          if (index < thisElementIndex) {
            if (draggedCenter < siblingCenter && indexToMoveTo == null) {
              indexToMoveTo = index
            }
          } else if (index > thisElementIndex) {
            if (draggedCenter > siblingCenter) {
              indexToMoveTo = index
            }
          }
        })

        const indexChanged = indexToMoveTo !== thisElementIndex

        if (indexToMoveTo != null && indexChanged) {
          return [
            updateElementIndex('permanent', targetedElement, indexToMoveTo),
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
