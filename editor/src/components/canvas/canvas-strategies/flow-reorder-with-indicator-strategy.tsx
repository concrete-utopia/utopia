import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { offsetPoint } from '../../../core/shared/math-utils'
import { absolute } from '../../../utils/utils'
import { CSSCursor } from '../canvas-types'
import * as EP from '../../../core/shared/element-path'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { FlowReorderDragOutline } from '../controls/flow-reorder-indicators'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getNewIndexAndInsertLine, getOptionalDisplayPropCommands } from './flow-reorder-helpers'
import { isFlowReorderConversionApplicable } from './flow-reorder-strategy'
import { isReorderAllowed } from './reorder-utils'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'

export const flowReorderWithIndicatorStrategy: CanvasStrategy = {
  id: 'FLOW_REORDER_WITH_INDICATOR',
  name: () => 'Reorder (Flow, I)',
  isApplicable: isFlowReorderConversionApplicable,
  controlsToRender: [
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
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: FlexReparentTargetIndicator,
      key: 'flex-reparent-target-indicator',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    return flowReorderWithIndicatorStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 999
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type !== 'DRAG') {
      return emptyStrategyApplicationResult
    }

    if (interactionState.interactionData.drag != null) {
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
      const target = selectedElements[0] // TODO MULTISELECT??

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

      const rawPointOnCanvas = offsetPoint(
        interactionState.interactionData.dragStart,
        interactionState.interactionData.drag,
      )

      const { newIndex, targetLineBeforeSibling } = getNewIndexAndInsertLine(
        strategyState.startingMetadata,
        siblingsOfTarget,
        rawPointOnCanvas,
        target,
        canvasState.scale,
      )

      const realNewIndex = newIndex > -1 ? newIndex : lastReorderIdx

      const flexReparentTargetLines =
        targetLineBeforeSibling != null ? [targetLineBeforeSibling] : []

      return strategyApplicationResult(
        [
          reorderElement('on-complete', target, absolute(realNewIndex)),
          updateHighlightedViews('mid-interaction', []),
          setCursorCommand('mid-interaction', CSSCursor.Move),
          wildcardPatch('mid-interaction', {
            canvas: {
              controls: {
                flexReparentTargetLines: {
                  $set: flexReparentTargetLines,
                },
              },
            },
          }),
        ],
        {
          lastReorderIdx: realNewIndex,
        },
      )
    } else {
      // Fallback for when the checks above are not satisfied.
      return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.Move)])
    }
  },
}
