import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
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
import type { InteractionSession } from '../interaction-state'
import {
  isValidFlowReorderTarget,
  singleAxisAutoLayoutSiblingDirections,
} from './flow-reorder-helpers'
import { retargetStrategyToTopMostFragmentLikeElement } from './fragment-like-helpers'
import { applyReorderCommon } from './reorder-utils'

export function flowReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): MoveStrategy | null {
  const originalTargets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const retargetedTargets = retargetStrategyToTopMostFragmentLikeElement(canvasState)

  if (retargetedTargets.length !== 1) {
    return null
  }
  const target = retargetedTargets[0]
  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    target,
  )

  const singleAxisAutolayoutDirection = singleAxisAutoLayoutSiblingDirections(
    target,
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
  )

  if (
    !MetadataUtils.isPositionedByFlow(elementMetadata) ||
    !isValidFlowReorderTarget(target, canvasState.startingMetadata) ||
    singleAxisAutolayoutDirection === 'non-single-axis-autolayout'
  ) {
    return null
  }

  return {
    strategy: {
      id: 'FLOW_REORDER',
      name: 'Reorder (Flow)',
      descriptiveLabel: 'Reordering Elements',
      icon: {
        category: 'modalities',
        type: 'reorder-large',
      },
      controlsToRender: [
        controlWithProps({
          control: ImmediateParentOutlines,
          props: { targets: originalTargets },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ImmediateParentBounds,
          props: { targets: originalTargets },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: DragOutlineControl,
          props: dragTargetsElementPaths(originalTargets),
          key: 'flow-reorder-drag-outline',
          show: 'visible-only-while-active',
        }),
      ], // Uses existing hooks in select-mode-hooks.tsx
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
              retargetedTargets,
              canvasState,
              interactionSession,
              customStrategyState,
              singleAxisAutolayoutDirection.direction,
              isValidFlowReorderTarget,
            )
      },
    },
    dragType: 'static',
  }
}
