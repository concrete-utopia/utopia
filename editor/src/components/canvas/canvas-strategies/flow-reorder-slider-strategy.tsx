import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasVector, mod } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { absolute } from '../../../utils/utils'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { FlowSliderControl } from '../controls/flow-slider-control'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getNewDisplayTypeForIndex, getOptionalDisplayPropCommands } from './flow-reorder-helpers'
import { isFlowReorderConversionApplicable } from './flow-reorder-strategy'
import { isReorderAllowed } from './reorder-utils'

const ReorderChangeThreshold = 32

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

      const newIndex = findNewIndex(
        unpatchedIndex,
        interactionState.interactionData.drag,
        siblingsOfTarget,
        'rounded-value',
      )

      const newDisplayType = getNewDisplayTypeForIndex(
        strategyState.startingMetadata,
        MetadataUtils.findElementByElementPath(strategyState.startingMetadata, target),
        siblingsOfTarget[newIndex],
      )

      return strategyApplicationResult(
        [
          reorderElement('always', target, absolute(newIndex)),
          setElementsToRerenderCommand(siblingsOfTarget),
          updateHighlightedViews('mid-interaction', []),
          ...getOptionalDisplayPropCommands(target, newDisplayType, 'with-auto-conversion'),
          setCursorCommand('mid-interaction', CSSCursor.ResizeEW),
        ],
        {
          lastReorderIdx: newIndex,
        },
      )
    } else {
      // Fallback for when the checks above are not satisfied.
      return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.ResizeEW)])
    }
  },
}

export function findNewIndex(
  startingIndex: number,
  drag: CanvasVector,
  siblings: Array<ElementPath>,
  shouldRound: 'rounded-value' | 'raw-value',
): number {
  const dragDelta = Math.abs(drag.x) > Math.abs(drag.y) ? drag.x : drag.y
  const reorderIndexPosition = dragDelta / ReorderChangeThreshold
  const indexOffset =
    shouldRound === 'rounded-value' ? Math.round(reorderIndexPosition) : reorderIndexPosition
  return mod(startingIndex + indexOffset, siblings.length)
}
