import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { toString } from '../../../../core/shared/element-path'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  MoveStrategy,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { honoursPropsPosition } from './absolute-utils'
import { retargetStrategyToChildrenOfContentAffectingElements } from './group-like-helpers'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  flattenSelection,
} from './shared-move-strategies-helpers'

export function absoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): MoveStrategy | null {
  const targets = retargetStrategyToChildrenOfContentAffectingElements(canvasState)

  const isApplicable =
    targets.length > 0 &&
    flattenSelection(targets).every((element) => {
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
          props: { targets: targets },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ImmediateParentBounds,
          props: { targets: targets },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ZeroSizedElementControls,
          props: { showAllPossibleElements: true },
          key: 'zero-size-control',
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
            targets,
            canvasState,
            interactionSession,
            getAdjustMoveCommands(targets, canvasState, interactionSession),
          )
        }
        // Fallback for when the checks above are not satisfied.
        return emptyStrategyApplicationResult
      },
    },
    dragType: 'absolute',
  }
}
