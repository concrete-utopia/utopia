import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { applyStaticReparent, ReparentTarget } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

export function baseReparentToFlexStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
): CanvasStrategyFactory {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
  ): CanvasStrategy | null => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const filteredSelectedElements = getDragTargets(selectedElements)
    if (
      filteredSelectedElements.length !== 1 ||
      interactionSession == null ||
      interactionSession.interactionData.type !== 'DRAG'
    ) {
      return null
    }

    const isOriginallyAbsolute =
      MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        filteredSelectedElements[0],
      )?.specialSizeMeasurements.position === 'absolute'

    const isOriginallyFlex =
      !isOriginallyAbsolute &&
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        filteredSelectedElements[0],
        canvasState.startingMetadata,
      )

    if (!isOriginallyAbsolute && !isOriginallyFlex) {
      return null
    }

    return {
      id: 'REPARENT_TO_FLEX',
      name: 'Reparent (Flex)',
      controlsToRender: [
        controlWithProps({
          control: DragOutlineControl,
          props: dragTargetsElementPaths(filteredSelectedElements),
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
      fitness: fitness,
      apply: () => {
        return applyStaticReparent(canvasState, interactionSession, reparentTarget, 'flex')
      },
    }
  }
}
