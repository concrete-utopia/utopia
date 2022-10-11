import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { honoursPropsPosition } from './absolute-utils'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  getDragTargets,
} from './shared-move-strategies-helpers'

export function absoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const isApplicable =
    selectedElements.length > 0 &&
    getDragTargets(selectedElements).every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        element,
      )
      return (
        elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
        honoursPropsPosition(canvasState, element)
      )
    })

  if (!isApplicable) {
    return null
  }
  return {
    id: 'ABSOLUTE_MOVE',
    name: 'Move',
    controlsToRender: [
      controlWithProps({
        control: ParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ParentBounds,
        props: { targets: selectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness:
      interactionSession?.interactionData.type === 'DRAG' &&
      interactionSession?.activeControl.type === 'BOUNDING_AREA'
        ? 1
        : 0,
    apply: () => {
      if (
        interactionSession?.interactionData.type === 'DRAG' &&
        interactionSession?.interactionData.drag != null
      ) {
        return applyMoveCommon(
          canvasState,
          interactionSession,
          getAdjustMoveCommands(canvasState, interactionSession),
        )
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}
