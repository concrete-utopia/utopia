import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { applyReorderCommon } from './reorder-utils'
import { InteractionSession } from '../interaction-state'
import { areAllSiblingsInOneDimensionFlexOrFlow } from './flow-reorder-helpers'

export function flexReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const element = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElements[0],
  )
  if (
    selectedElements.length !== 1 ||
    !(
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        selectedElements[0],
        canvasState.startingMetadata,
      ) && !element?.specialSizeMeasurements.hasTransform
    )
  ) {
    return null
  }

  if (
    areAllSiblingsInOneDimensionFlexOrFlow(selectedElements[0], canvasState.startingMetadata) ===
    'non-1d-static'
  ) {
    return null
  }

  const parentFlexDirection = element?.specialSizeMeasurements.parentFlexDirection
  const reorderDirection = parentFlexDirection === 'column' ? 'vertical' : 'horizontal'
  return {
    id: 'FLEX_REORDER',
    name: 'Reorder (Flex)',
    controlsToRender: [
      controlWithProps({
        control: DragOutlineControl,
        props: dragTargetsElementPaths(selectedElements),
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: selectedElements },
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
            reorderDirection,
            MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout,
          )
    },
  }
}
