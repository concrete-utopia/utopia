import { aperture, mapDropNulls, safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { adjustNumberProperty, applyValuesAtPath, wildcardPatch } from '../commands/commands'
import { CanvasStrategy } from '../../../interactions_proposal'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { FlexGapControls } from '../controls/select-mode/flex-gap-controls'

export const flexGapStrategy: CanvasStrategy = {
  name: 'Change Flex Gap',
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
  controlsToRender: [FlexGapControls],
  fitness: (canvasState, interactionState) => {
    return flexGapStrategy.isApplicable(canvasState, interactionState) &&
      interactionState.interactionData.type === 'DRAG'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'FLEX_GAP_HANDLE'
    ) {
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const targetParent = MetadataUtils.getParent(canvasState.metadata, targetedElement)
      const gapPropPath = stylePropPathMappingFn('gap', ['style'])

      if (targetParent !== null) {
        const flexDirection = MetadataUtils.getFlexDirection(targetParent)
        let gapChange: number = 0
        if (flexDirection.startsWith('row')) {
          gapChange = interactionState.interactionData.drag?.x ?? 0
        } else {
          gapChange = interactionState.interactionData.drag?.y ?? 0
        }
        const adjustProperty = adjustNumberProperty(
          'permanent',
          targetedElement,
          gapPropPath,
          gapChange,
        )

        // Identify the siblings so that the metadata gets updated for those as well,
        // which should result in the gap controls also being updated.
        const siblingsOfTarget = MetadataUtils.getSiblings(
          canvasState.metadata,
          targetedElement,
        ).map((metadata) => metadata.elementPath)
        return [
          adjustProperty,
          wildcardPatch('transient', {
            highlightedViews: {
              $set: [],
            },
            canvas: {
              domWalkerAdditionalElementsToUpdate: {
                $set: siblingsOfTarget,
              },
            },
          }),
        ]
      }
    }

    // Fallback for when the checks above are not satisfied.
    return []
  },
}
