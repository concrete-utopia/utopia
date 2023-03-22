import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
import {
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  MoveStrategy,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  isValidFlowReorderTarget,
  singleAxisAutoLayoutSiblingDirections,
} from './flow-reorder-helpers'
import { applyReorderCommon } from './reorder-utils'

export function flowReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): MoveStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }
  const target = selectedElements[0]

  const singleAxisAutolayoutDirection = singleAxisAutoLayoutSiblingDirections(
    target,
    canvasState.startingMetadata,
  )

  if (
    !MetadataUtils.isPositionedByFlow(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      target,
    ) ||
    !isValidFlowReorderTarget(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      target,
    ) ||
    singleAxisAutolayoutDirection === 'non-single-axis-autolayout'
  ) {
    return null
  }

  return {
    strategy: {
      id: 'FLOW_REORDER',
      name: 'Reorder (Flow)',
      controlsToRender: [
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
        controlWithProps({
          control: DragOutlineControl,
          props: dragTargetsElementPaths(selectedElements),
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
