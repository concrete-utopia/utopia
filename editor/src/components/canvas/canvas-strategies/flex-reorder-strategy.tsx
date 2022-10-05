import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasStrategy, getTargetPathsFromInteractionTarget } from './canvas-strategy-types'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { ParentOutlines } from '../controls/parent-outlines'
import { ParentBounds } from '../controls/parent-bounds'
import { applyReorderCommon } from './reorder-utils'

export const flexReorderStrategy: CanvasStrategy = {
  id: 'FLEX_REORDER',
  name: () => 'Reorder (Flex)',
  isApplicable: (canvasState, interactionSession, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        selectedElements[0],
        metadata,
      )
    } else {
      return false
    }
  },
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ],
  fitness: (canvasState, interactionSession, customStrategyState) => {
    return flexReorderStrategy.isApplicable(
      canvasState,
      interactionSession,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionSession, customStrategyState) => {
    return applyReorderCommon(
      canvasState,
      interactionSession,
      customStrategyState,
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout,
    )
  },
}
