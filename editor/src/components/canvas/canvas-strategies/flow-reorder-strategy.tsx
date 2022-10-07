import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { FlowReorderDragOutline } from '../controls/flow-reorder-indicators'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { areAllSiblingsInOneDimension, isValidFlowReorderTarget } from './flow-reorder-helpers'
import { InteractionSession } from './interaction-state'
import { applyReorderCommon } from './reorder-utils'

export function flowReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }
  const target = selectedElements[0]
  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    target,
  )
  if (
    !MetadataUtils.isPositionedByFlow(elementMetadata) ||
    !isValidFlowReorderTarget(target, canvasState.startingMetadata) ||
    !areAllSiblingsInOneDimension(target, canvasState.startingMetadata)
  ) {
    return null
  }

  return {
    id: 'FLOW_REORDER',
    name: 'Reorder (Flow)',
    controlsToRender: [
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
      controlWithProps({
        control: FlowReorderDragOutline,
        props: {},
        key: 'flow-reorder-drag-outline',
        show: 'visible-only-while-active',
      }),
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? 3
        : 0,
    apply: () => {
      return interactionSession == null
        ? emptyStrategyApplicationResult
        : applyReorderCommon(
            canvasState,
            interactionSession,
            customStrategyState,
            isValidFlowReorderTarget,
          )
    },
  }
}
