import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { AllElementProps } from '../../editor/store/editor-state'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import {
  CanvasStrategy,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { applyFlexReparent, findReparentStrategy } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

export const absoluteReparentToFlexStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT_TO_FLEX',
  name: 'Absolute Reparent to Flex',
  isApplicable: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ): boolean {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.interactionData.modifiers.cmd
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
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
  ],
  fitness: function (
    canvasState: InteractionCanvasState,
    interactionState: InteractionSession,
    strategyState: StrategyState,
  ): number {
    // All 4 reparent strategies use the same fitness function findReparentStrategy
    const reparentStrategy = findReparentStrategy(
      canvasState,
      interactionState,
      strategyState,
    ).strategy
    if (reparentStrategy === 'ABSOLUTE_REPARENT_TO_FLEX') {
      return 3
    }
    return 0
  },
  apply: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyState,
  ): StrategyApplicationResult {
    return applyFlexReparent('strip-absolute-props', canvasState, interactionSession, strategyState)
  },
}
