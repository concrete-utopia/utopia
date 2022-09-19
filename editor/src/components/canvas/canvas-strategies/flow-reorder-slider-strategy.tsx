import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasVector, mod, pointDifference } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { absolute } from '../../../utils/utils'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { FlowSliderControl } from '../controls/flow-slider-control'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getNewDisplayTypeForIndex, getOptionalDisplayPropCommands } from './flow-reorder-helpers'
import { isFlowReorderConversionApplicable } from './flow-reorder-strategy'
import { DragInteractionData, StrategyState } from './interaction-state'
import { isReorderAllowed } from './reorder-utils'

const ReorderChangeThreshold = 80
const ResetTimer = 50

export const flowReorderSliderStategy: CanvasStrategy = {
  id: 'FLOW_REORDER_SLIDER',
  name: 'Reorder (Slider)',
  isApplicable: (canvasState, interactionState, metadata, allElementProps) => {
    return isFlowReorderConversionApplicable(
      canvasState,
      interactionState,
      metadata,
      allElementProps,
      'no-filter',
    )
  },
  controlsToRender: [
    {
      control: FlowSliderControl,
      key: 'flow-slider-control',
      show: 'always-visible',
    },
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    return flowReorderSliderStategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'FLOW_SLIDER'
      ? 100
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type !== 'DRAG') {
      return emptyStrategyApplicationResult
    }

    if (interactionState.interactionData.drag != null) {
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
      const target = selectedElements[0]

      const siblingsOfTarget = MetadataUtils.getSiblings(
        strategyState.startingMetadata,
        target,
      ).map((element) => element.elementPath)

      if (!isReorderAllowed(siblingsOfTarget)) {
        return strategyApplicationResult(
          [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
          {},
          'failure',
        )
      }

      const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
      const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

      const { newIndex, flowReorderIndexPosition } = findNewIndex(
        lastReorderIdx,
        interactionState.interactionData.drag,
        siblingsOfTarget,
        strategyState,
      )

      const newDisplayType = getNewDisplayTypeForIndex(
        strategyState.startingMetadata,
        MetadataUtils.findElementByElementPath(strategyState.startingMetadata, target),
        siblingsOfTarget[newIndex],
      )

      const shouldReorder = newIndex !== lastReorderIdx

      const lastReorderHappened = shouldReorder
        ? Date.now()
        : strategyState.customStrategyState.flowLastReorderHappened ?? null

      return strategyApplicationResult(
        [
          reorderElement('always', target, absolute(newIndex)),
          setElementsToRerenderCommand(siblingsOfTarget),
          updateHighlightedViews('mid-interaction', []),
          ...getOptionalDisplayPropCommands(target, newDisplayType, 'with-auto-conversion'),
          setCursorCommand('mid-interaction', CSSCursor.ResizeEW),
          wildcardPatch('mid-interaction', {
            canvas: {
              controls: {
                flowReorderIndexPosition: { $set: flowReorderIndexPosition }, // this shows the indicator between elements
              },
            },
          }),
        ],
        {
          lastReorderIdx: newIndex,
          flowDragDeltaSinceLastReorder: maybeResetFlowReorderDragDelta(
            shouldReorder,
            interactionState.interactionData,
            strategyState,
          ),
          flowLastReorderHappened: lastReorderHappened,
        },
      )
    } else {
      // Fallback for when the checks above are not satisfied.
      return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.ResizeEW)])
    }
  },
}

function maybeResetFlowReorderDragDelta(
  shouldReorder: boolean,
  interactionData: DragInteractionData,
  strategyState: StrategyState,
): CanvasVector | null {
  // drag vector resets after a reorder is triggered to ensure the same drag threshold when switching directions
  if (shouldReorder) {
    return interactionData.drag
  }
  // this timer also resets the drag vector until the animation finishes to keep the indicator in good position
  if (
    interactionData.globalTime - (strategyState.customStrategyState.flowLastReorderHappened ?? 0) <
    ResetTimer
  ) {
    return interactionData.drag
  }

  return strategyState.customStrategyState.flowDragDeltaSinceLastReorder
}

function findNewIndex(
  lastReorderIdx: number,
  drag: CanvasVector,
  siblings: Array<ElementPath>,
  strategyState: StrategyState,
): {
  newIndex: number
  flowReorderIndexPosition: number
} {
  // drag vector resets after a reorder is triggered to ensure the same drag threshold when switching directions
  const dragVectorSinceLastReorder =
    strategyState.customStrategyState.flowDragDeltaSinceLastReorder != null
      ? pointDifference(strategyState.customStrategyState.flowDragDeltaSinceLastReorder, drag)
      : drag

  const reorderIndexPositionFraction = dragVectorSinceLastReorder.x / ReorderChangeThreshold
  const indexOffset = Math.round(reorderIndexPositionFraction)
  return {
    newIndex: mod(lastReorderIdx + indexOffset, siblings.length),
    flowReorderIndexPosition: reorderIndexPositionFraction,
  }
}
