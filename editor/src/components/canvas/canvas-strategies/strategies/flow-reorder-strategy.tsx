import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import {
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  MoveStrategy,
} from '../canvas-strategy-types'
import {
  isValidFlowReorderTarget,
  singleAxisAutoLayoutSiblingDirections,
} from './flow-reorder-helpers'
import { InteractionSession } from '../interaction-state'
import { applyReorderCommon } from './reorder-utils'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
import {
  boundingRectangleArray,
  nullIfInfinity,
  offsetPoint,
  rectContainsPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'

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
  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    target,
  )

  const singleAxisAutolayoutDirection = singleAxisAutoLayoutSiblingDirections(
    target,
    canvasState.startingMetadata,
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
      fitness: getFitness(canvasState, interactionSession, target),
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

function getFitness(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  target: ElementPath,
): number {
  if (
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.activeControl.type === 'BOUNDING_AREA'
  ) {
    if (interactionSession.interactionData.drag == null) {
      return 1
    }

    const autoLayoutSiblings = MetadataUtils.getSiblingsParticipatingInAutolayoutUnordered(
      canvasState.startingMetadata,
      target,
    )
    const autoLayoutSiblingsFrames = autoLayoutSiblings.map((e) => nullIfInfinity(e.globalFrame))
    const autoLayoutSiblingsBounds = boundingRectangleArray(autoLayoutSiblingsFrames)

    const pointOnCanvas = offsetPoint(
      interactionSession.interactionData.dragStart,
      interactionSession.interactionData.drag,
    )

    const isInsideBoundingBoxOfSiblings =
      autoLayoutSiblingsBounds != null && rectContainsPoint(autoLayoutSiblingsBounds, pointOnCanvas)

    return isInsideBoundingBoxOfSiblings ? 1 : 0
  }

  return 0
}
