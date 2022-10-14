import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { DragOutlineControl } from '../../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { applyFlexReparent, ReparentTarget } from './reparent-strategy-helpers'

export function baseFlexReparentToFlexStrategy(
  reparentTarget: ReparentTarget,
): CanvasStrategyFactory {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
  ): CanvasStrategy | null => {
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
      id: 'FLEX_REPARENT_TO_FLEX',
      name: 'Reparent (Flex)',
      controlsToRender: [
        controlWithProps({
          control: DragOutlineControl,
          props: { targets: selectedElements },
          key: 'ghost-outline-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ParentOutlines,
          props: { targetParent: reparentTarget.newParent },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ParentBounds,
          props: { targetParent: reparentTarget.newParent },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: FlexReparentTargetIndicator,
          props: {},
          key: 'flex-reparent-target-indicator',
          show: 'visible-only-while-active',
        }),
      ],
      fitness: 3,
      apply: () => {
        return interactionSession == null
          ? emptyStrategyApplicationResult
          : applyFlexReparent('do-not-strip-props', canvasState, interactionSession, reparentTarget)
      },
    }
  }
}
