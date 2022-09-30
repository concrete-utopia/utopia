import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { AllElementProps } from '../../editor/store/editor-state'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import {
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState, StrategyStateNew } from './interaction-state'
import { applyFlexReparent, getFitnessForReparentStrategy } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

export const absoluteReparentToFlexStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT_TO_FLEX',
  name: () => 'Reparent (Flex)',
  isApplicable: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ): boolean {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      selectedElements.length === 1 &&
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG'
    ) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      if (filteredSelectedElements.length === 1) {
        const elementMetadata = MetadataUtils.findElementByElementPath(
          metadata,
          filteredSelectedElements[0],
        )

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      }
    }
    return false
  },
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
    {
      control: FlexReparentTargetIndicator,
      key: 'flex-reparent-target-indicator',
      show: 'visible-only-while-active',
    },
  ],
  fitness: function (
    canvasState: InteractionCanvasState,
    interactionState: InteractionSession,
    strategyState: StrategyStateNew,
  ): number {
    // All 4 reparent strategies use the same fitness function getFitnessForReparentStrategy
    return getFitnessForReparentStrategy(
      'ABSOLUTE_REPARENT_TO_FLEX',
      canvasState,
      interactionState,
      strategyState,
      'use-strict-bounds',
    )
  },
  apply: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyStateNew,
  ): StrategyApplicationResult {
    return applyFlexReparent('strip-absolute-props', canvasState, interactionSession)
  },
}
