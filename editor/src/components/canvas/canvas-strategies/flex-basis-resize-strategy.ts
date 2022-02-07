import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { last, safeIndex } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasStrategy } from '../../../interactions_proposal'
import { deleteProperty, setProperty, wildcardPatch } from '../commands/commands'
import { FlexResizeControls } from '../controls/select-mode/flex-resize-control'

function getSiblingSpacingFromEdge(
  metadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
): number | null {
  // While the furthest sibling is not snapping or about to snap?
  const parentMetadata = MetadataUtils.getParent(metadata, selectedElement)
  if (parentMetadata == null) {
    return null
  }
  const siblings = MetadataUtils.getSiblings(metadata, selectedElement)
  const lastSibling = forceNotNull('Should be able to get the last sibling.', last(siblings))

  if (parentMetadata.globalFrame != null && lastSibling.globalFrame != null) {
    const parentRight = parentMetadata.globalFrame.x + parentMetadata.globalFrame.width
    const siblingRight = lastSibling.globalFrame.x + lastSibling.globalFrame.width
    return siblingRight - parentRight
  } else {
    return null
  }
}

export const flexBasisResizeStrategy: CanvasStrategy = {
  name: 'Flex Basis Resize',
  strategyGroups: new Set(['flex-basis-grow']),
  isApplicable: (canvasState, interactionState) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedElement = safeIndex(canvasState.selectedElements, 0)

      if (selectedElement != null) {
        const isFlexLayouted = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
          MetadataUtils.findElementByElementPath(canvasState.metadata, selectedElement),
        )
        if (isFlexLayouted) {
          return true
        }
      }
    }
    return false
  },
  controlsToRender: [
    { control: FlexResizeControls, key: 'FlexResizeControls', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    const applicable = flexBasisResizeStrategy.isApplicable(canvasState, interactionState)
    if (
      applicable &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const selectedElement = forceNotNull(
        'isApplicable should prevent null.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const originalSiblingSpacing = getSiblingSpacingFromEdge(
        sessionState.startingMetadata,
        selectedElement,
      )
      if (originalSiblingSpacing != null) {
        let spacing: number = originalSiblingSpacing
        const intData = interactionState.interactionData
        if (intData.drag != null) {
          spacing += intData.drag.x
        }
        if (Math.abs(spacing) > 5) {
          return 10
        }
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    // only apply after a certain treshold IF we hadn't already passed that treshold once
    const draggedElement = safeIndex(canvasState.selectedElements, 0)
    if (draggedElement == null || interactionState.interactionData.type !== 'DRAG') {
      return []
    }

    const originalMetadata = MetadataUtils.findElementByElementPath(
      sessionState.startingMetadata,
      draggedElement,
    )
    const newFlexBasis =
      (originalMetadata?.localFrame?.width ?? 0) + (interactionState?.interactionData.drag?.x ?? 0)

    // Make element flex-basis.
    const flexBasisMove = setProperty(
      'permanent',
      draggedElement,
      PP.create(['style', 'flexBasis']),
      jsxAttributeValue(newFlexBasis, emptyComments),
    )
    const deleteFlexGrow = deleteProperty(
      'permanent',
      draggedElement,
      PP.create(['style', 'flexGrow']),
    )

    return [
      flexBasisMove,
      deleteFlexGrow,
      wildcardPatch('transient', {
        canvas: {
          controls: {
            animatedPlaceholderTargetUids: {
              $set: [EP.toUid(draggedElement)],
            },
          },
        },
      }),
    ]
  },
}

export const flexGrowResizeStrategy: CanvasStrategy = {
  name: 'Flex Grow Resize',
  strategyGroups: new Set(['flex-basis-grow']),
  isApplicable: (canvasState, interactionState) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedElement = safeIndex(canvasState.selectedElements, 0)

      if (selectedElement != null) {
        const isFlexLayouted = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
          MetadataUtils.findElementByElementPath(canvasState.metadata, selectedElement),
        )
        if (isFlexLayouted) {
          return true
        }
      }
    }
    return false
  },
  controlsToRender: [
    { control: FlexResizeControls, key: 'FlexResizeControls', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    const applicable = flexGrowResizeStrategy.isApplicable(canvasState, interactionState)
    if (
      applicable &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const selectedElement = forceNotNull(
        'isApplicable should prevent null.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const originalSiblingSpacing = getSiblingSpacingFromEdge(
        sessionState.startingMetadata,
        selectedElement,
      )
      if (originalSiblingSpacing != null) {
        let spacing: number = originalSiblingSpacing
        const intData = interactionState.interactionData
        if (intData.drag != null) {
          spacing += intData.drag.x
        }
        if (Math.abs(spacing) <= 5) {
          return 10
        }
      }
    }
    return 0
  },
  apply: (canvasState, interactionState) => {
    // only apply after a certain treshold IF we hadn't already passed that treshold once
    const draggedElement = safeIndex(canvasState.selectedElements, 0)
    if (draggedElement == null || interactionState.interactionData.type !== 'DRAG') {
      return []
    }

    // Make element flex-basis.
    const setFlexGrow = setProperty(
      'permanent',
      draggedElement,
      PP.create(['style', 'flexGrow']),
      jsxAttributeValue(1, emptyComments),
    )
    const deleteFlexBasis = deleteProperty(
      'permanent',
      draggedElement,
      PP.create(['style', 'flexBasis']),
    )

    return [
      setFlexGrow,
      deleteFlexBasis,
      wildcardPatch('transient', {
        canvas: {
          controls: {
            animatedPlaceholderTargetUids: {
              $set: [EP.toUid(draggedElement)],
            },
          },
        },
      }),
    ]
  },
}
