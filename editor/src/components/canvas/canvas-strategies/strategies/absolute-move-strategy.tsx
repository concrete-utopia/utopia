import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import { getDescriptiveStrategyLabelWithRetargetedPaths } from '../canvas-strategies'
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
import { honoursPropsToPositionElement } from './absolute-utils'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './fragment-like-helpers'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  flattenSelection,
} from './shared-move-strategies-helpers'
import * as EP from '../../../../core/shared/element-path'

export type ShouldRunApplicabilityCheck =
  | 'run-applicability-check'
  | 'do-not-run-applicability-check'

export function absoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
  runApplicabilityCheck: ShouldRunApplicabilityCheck = 'run-applicability-check',
): MoveStrategy | null {
  const originalTargets = flattenSelection(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )
  const { pathsWereReplaced, paths: retargetedTargets } =
    retargetStrategyToChildrenOfFragmentLikeElements(canvasState)

  const isApplicable =
    retargetedTargets.length > 0 &&
    flattenSelection(retargetedTargets).every((element) => {
      return honoursPropsToPositionElement(canvasState, element)
    })

  if (runApplicabilityCheck === 'run-applicability-check' && !isApplicable) {
    return null
  }
  return {
    strategy: {
      id: 'ABSOLUTE_MOVE',
      name: 'Move',
      descriptiveLabel: getDescriptiveStrategyLabelWithRetargetedPaths(
        'Moving Absolute Elements',
        pathsWereReplaced,
      ),
      icon: {
        category: 'modalities',
        type: 'moveabs-large',
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
          control: ZeroSizedElementControls,
          props: { showAllPossibleElements: true },
          key: 'zero-size-control',
          show: 'visible-only-while-active',
        }),
      ], // Uses existing hooks in select-mode-hooks.tsx
      fitness:
        interactionSession?.interactionData.type === 'DRAG' &&
        interactionSession?.activeControl.type === 'BOUNDING_AREA'
          ? 2 // NOTE: this is potentially a good candidate to look at in case some other strategies fail to trigger unexpectedly
          : 0,
      apply: () => {
        if (
          interactionSession?.interactionData.type === 'DRAG' &&
          interactionSession?.interactionData.drag != null
        ) {
          return applyMoveCommon(
            originalTargets,
            retargetedTargets,
            canvasState,
            interactionSession,
            getAdjustMoveCommands(retargetedTargets, canvasState, interactionSession),
            customStrategyState.action ?? 'move',
          )
        }
        // Fallback for when the checks above are not satisfied.
        return emptyStrategyApplicationResult
      },
    },
    dragType: 'absolute',
  }
}
