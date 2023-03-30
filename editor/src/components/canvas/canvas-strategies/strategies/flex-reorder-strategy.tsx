import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  MoveStrategy,
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
import { retargetStrategyToTopMostGroupLikeElement } from './group-like-helpers'

export function flexReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): MoveStrategy | null {
  const originalTargets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const retargetedTargets = retargetStrategyToTopMostGroupLikeElement(canvasState)
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

  if (!areAllSiblingsInOneDimensionFlexOrFlow(retargetedTargets[0], canvasState.startingMetadata)) {
    return null
  }

  const parentFlexDirection = element?.specialSizeMeasurements.parentFlexDirection
  const reorderDirection = parentFlexDirection === 'column' ? 'vertical' : 'horizontal'
  return {
    strategy: {
      id: 'FLEX_REORDER',
      name: 'Reorder (Flex)',
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
          ? 1
          : 0,
      apply: () => {
        return interactionSession == null
          ? emptyStrategyApplicationResult
          : applyReorderCommon(
              originalTargets,
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
