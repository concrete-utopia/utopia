import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mod } from '../../../core/shared/math-utils'
import { absolute } from '../../../utils/utils'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { FlowSliderControl } from '../controls/flow-slider-control'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getNewDisplayTypeForIndex, getOptionalDisplayPropCommands } from './flow-reorder-helpers'
import { isFlowReorderConversionApplicable } from './flow-reorder-strategy'
import { isReorderAllowed } from './reorder-utils'

export const flowReorderSliderStategy: CanvasStrategy = {
  id: 'FLOW_REORDER_SLIDER',
  name: 'Reorder (Slider)',
  isApplicable: (canvasState, interactionState, strategyState, allElementProps) => {
    return isFlowReorderConversionApplicable(
      canvasState,
      interactionState,
      strategyState,
      allElementProps,
      'no-filter',
    )
  },
  controlsToRender: [
    // {
    //   control: FlowSliderControl,
    //   key: 'flow-slider-control',
    //   show: 'always-visible',
    // },
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

      const indexOffset = Math.round(interactionState.interactionData.drag.x / 40)

      const realNewIndex = mod(unpatchedIndex + indexOffset, siblingsOfTarget.length)

      const newDisplayType = getNewDisplayTypeForIndex(
        strategyState.startingMetadata,
        MetadataUtils.findElementByElementPath(strategyState.startingMetadata, target),
        siblingsOfTarget[realNewIndex],
      )

      return strategyApplicationResult(
        [
          reorderElement('always', target, absolute(realNewIndex)),
          setElementsToRerenderCommand(siblingsOfTarget),
          updateHighlightedViews('mid-interaction', []),
          ...getOptionalDisplayPropCommands(target, newDisplayType, 'with-auto-conversion'),
          setCursorCommand('mid-interaction', CSSCursor.ResizeEW),
        ],
        {
          lastReorderIdx: realNewIndex,
        },
      )
    } else {
      // Fallback for when the checks above are not satisfied.
      return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.ResizeEW)])
    }
  },
}
