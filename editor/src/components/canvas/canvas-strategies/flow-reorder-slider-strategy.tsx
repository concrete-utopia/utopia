import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
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
import {
  findNewIndex,
  getNewDisplayTypeForIndex,
  getOptionalDisplayPropCommands,
} from './flow-reorder-helpers'
import { isReorderAllowed } from './reorder-utils'

export const flowReorderSliderStategy: CanvasStrategy = {
  id: 'FLOW_REORDER_SLIDER',
  name: () => 'Reorder (Slider)',
  isApplicable: (canvasState, interactionSession, metadata, allElementProps) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 1) {
      const target = selectedElements[0]
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, target)
      const siblings = MetadataUtils.getSiblings(metadata, target)
      if (siblings.length > 1 && MetadataUtils.isPositionedByFlow(elementMetadata)) {
        return true
      }
    }
    return false
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

    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const target = selectedElements[0]

    const siblingsOfTarget = MetadataUtils.getSiblings(strategyState.startingMetadata, target).map(
      (element) => element.elementPath,
    )

    if (!isReorderAllowed(siblingsOfTarget)) {
      return strategyApplicationResult(
        [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
        {},
        'failure',
      )
    }

    if (interactionState.interactionData.drag != null) {
      const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))

      const newIndex = findNewIndex(
        unpatchedIndex,
        interactionState.interactionData.drag,
        siblingsOfTarget,
        'rounded-value',
      )

      const newDisplayType = getNewDisplayTypeForIndex(
        strategyState.startingMetadata,
        target,
        siblingsOfTarget[newIndex],
      )

      return strategyApplicationResult(
        [
          reorderElement('always', target, absolute(newIndex)),
          setElementsToRerenderCommand(siblingsOfTarget),
          updateHighlightedViews('mid-interaction', []),
          ...getOptionalDisplayPropCommands(target, newDisplayType),
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
