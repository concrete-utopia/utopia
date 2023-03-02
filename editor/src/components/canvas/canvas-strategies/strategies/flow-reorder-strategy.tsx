import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasRectangle,
  nullIfInfinity,
  offsetPoint,
  rectContainsPoint,
} from '../../../../core/shared/math-utils'
import { memoize } from '../../../../core/shared/memoize'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { AutoLayoutSiblingsOutline } from '../../controls/autolayout-siblings-outline'
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
  const autoLayoutSiblingsBounds = getAutoLayoutSiblingsBounds(canvasState.startingMetadata, target)

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
        controlWithProps({
          control: AutoLayoutSiblingsOutline,
          props: { bounds: autoLayoutSiblingsBounds },
          key: 'autolayout-siblings-outline',
          show: 'always-visible',
        }),
      ], // Uses existing hooks in select-mode-hooks.tsx
      fitness: getFitness(interactionSession, autoLayoutSiblingsBounds),
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

export const getAutoLayoutSiblingsBounds = memoize(getAutoLayoutSiblingsBoundsInner, { maxSize: 1 })

function getAutoLayoutSiblingsBoundsInner(
  jsxMetadata: ElementInstanceMetadataMap,
  target: ElementPath,
): CanvasRectangle | null {
  const autoLayoutSiblings = MetadataUtils.getSiblingsParticipatingInAutolayoutUnordered(
    jsxMetadata,
    target,
  )
  const autoLayoutSiblingsFrames = autoLayoutSiblings.map((e) => nullIfInfinity(e.globalFrame))
  return boundingRectangleArray(autoLayoutSiblingsFrames)
}

function getFitness(
  interactionSession: InteractionSession | null,
  autoLayoutSiblingsBounds: CanvasRectangle | null,
): number {
  if (
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.activeControl.type === 'BOUNDING_AREA'
  ) {
    if (interactionSession.interactionData.drag == null) {
      return 1
    }

    const pointOnCanvas = offsetPoint(
      interactionSession.interactionData.dragStart,
      interactionSession.interactionData.drag,
    )

    const isInsideBoundingBoxOfSiblings =
      autoLayoutSiblingsBounds != null && rectContainsPoint(autoLayoutSiblingsBounds, pointOnCanvas)

    return isInsideBoundingBoxOfSiblings ? 1 : 0.1
  }

  return 0
}
