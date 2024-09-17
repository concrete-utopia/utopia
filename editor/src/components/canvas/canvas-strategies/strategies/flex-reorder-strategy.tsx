import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type {
  CustomStrategyState,
  InteractionCanvasState,
  MoveStrategy,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from '../canvas-strategy-types'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { applyReorderCommon } from './reorder-utils'
import type { InteractionSession } from '../interaction-state'
import { areAllSiblingsInOneDimensionFlexOrFlow } from './flow-reorder-helpers'
import { retargetStrategyToTopMostFragmentLikeElement } from './fragment-like-helpers'

export function flexReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): MoveStrategy | null {
  const originalTargets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const retargetedTargets = retargetStrategyToTopMostFragmentLikeElement(canvasState)
  const element = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    retargetedTargets[0],
  )
  if (
    retargetedTargets.length !== 1 ||
    !(
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        retargetedTargets[0],
        canvasState.startingMetadata,
      ) && !element?.specialSizeMeasurements.hasTransform
    )
  ) {
    return null
  }

  if (
    !areAllSiblingsInOneDimensionFlexOrFlow(
      retargetedTargets[0],
      canvasState.startingMetadata,
      canvasState.startingElementPathTree,
    )
  ) {
    return null
  }

  const FlexReorderFitness = 1.4 // needs to be higher than WeightWithoutExistingOffset in relative-move-strategy, but lower than DragConversionWeight in convert-to-absolute-and-move-strategy

  const parentFlexDirection = element?.specialSizeMeasurements.parentFlexDirection
  const reorderDirection = parentFlexDirection === 'column' ? 'vertical' : 'horizontal'
  return {
    strategy: {
      id: 'FLEX_REORDER',
      name: 'Reorder (Flex)',
      descriptiveLabel: 'Reordering Flex Elements',
      icon: {
        category: 'modalities',
        type: 'reorder-large',
      },
      controlsToRender: [
        controlWithProps({
          control: DragOutlineControl,
          props: dragTargetsElementPaths(originalTargets),
          key: 'ghost-outline-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ImmediateParentOutlines,
          props: { targets: originalTargets }, // question to Berci: should this be the original target? or the retargeted _ancestor_?
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ImmediateParentBounds,
          props: { targets: originalTargets },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
      ],
      fitness:
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'BOUNDING_AREA'
          ? FlexReorderFitness
          : 0,
      apply: () => {
        return interactionSession == null
          ? emptyStrategyApplicationResult
          : applyReorderCommon(
              retargetedTargets,
              canvasState,
              interactionSession,
              customStrategyState,
              reorderDirection,
              MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout,
            )
      },
    },
    dragType: 'static',
  }
}
