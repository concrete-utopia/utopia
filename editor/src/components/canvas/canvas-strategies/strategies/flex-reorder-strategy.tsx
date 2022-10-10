import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { DragOutlineControl } from '../../controls/select-mode/drag-outline-control'
import { ParentOutlines } from '../../controls/parent-outlines'
import { ParentBounds } from '../../controls/parent-bounds'
import { applyReorderCommon } from './reorder-utils'
import { InteractionSession } from '../interaction-state'

export function flexReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length !== 1 ||
    !MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedElements[0],
      canvasState.startingMetadata,
    )
  ) {
    return null
  }
  return {
    id: 'FLEX_REORDER',
    name: 'Reorder (Flex)',
    controlsToRender: [
      controlWithProps({
        control: DragOutlineControl,
        props: {},
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ParentOutlines,
        props: {},
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ParentBounds,
        props: {},
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? 1
        : 0,
    apply: () => {
      return interactionSession == null
        ? emptyStrategyApplicationResult
        : applyReorderCommon(
            canvasState,
            interactionSession,
            customStrategyState,
            MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout,
          )
    },
  }
}
