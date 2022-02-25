import { safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { adjustNumberProperty, wildcardPatch } from '../commands/commands'
import { CanvasStrategy } from './canvas-strategy-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { FlexGapControls } from '../controls/select-mode/flex-gap-controls'

export const flexGapStrategy: CanvasStrategy = {
  name: 'Change Flex Gap',
  strategyGroups: new Set(),
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedView = canvasState.selectedElements[0]
      const selectedMetadata = MetadataUtils.findElementByElementPath(metadata, selectedView)
      return selectedMetadata?.specialSizeMeasurements.parentLayoutSystem === 'flex'
    }
    return false
  },
  controlsToRender: [
    { control: FlexGapControls, key: 'flex-gap-controls', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return flexGapStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'FLEX_GAP_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'FLEX_GAP_HANDLE'
    ) {
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const targetParent = MetadataUtils.getParent(sessionState.startingMetadata, targetedElement)
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
          targetParent.elementPath,
          gapPropPath,
          gapChange,
          true,
        )

        // Identify the siblings so that the metadata gets updated for those as well,
        // which should result in the gap controls also being updated.
        const siblingsOfTarget = MetadataUtils.getSiblings(
          sessionState.startingMetadata,
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
