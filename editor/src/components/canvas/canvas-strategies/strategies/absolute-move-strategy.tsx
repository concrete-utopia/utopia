import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { honoursPropsPosition } from './absolute-utils'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  MoveStrategy,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  getDragTargets,
} from './shared-move-strategies-helpers'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'

export function absoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): MoveStrategy | null {
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
    strategy: {
      id: 'ABSOLUTE_MOVE',
      name: 'Move',
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
          control: ZeroSizedElementControls,
          props: { showAllPossibleElements: true },
          key: 'zero-size-control',
          show: 'visible-only-while-active',
        }),
        {
          control: DragOutlineControl,
          props: dragTargetsElementPaths(selectedElements),
          key: 'ghost-outline-control',
          show: 'visible-only-while-active',
        },
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
    },
    dragType: 'absolute',
  }
}
