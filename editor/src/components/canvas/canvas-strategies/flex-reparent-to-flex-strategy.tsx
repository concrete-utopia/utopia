import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { canvasPoint, offsetPoint, rectContainsPoint } from '../../../core/shared/math-utils'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CSSCursor } from '../canvas-types'
import { getReparentTarget } from '../canvas-utils'
import { CanvasCommand, foldAndApplyCommandsInner, TransientOrNot } from '../commands/commands'
import { reorderElement } from '../commands/reorder-element-command'
import { reparentElement } from '../commands/reparent-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateFunctionCommand } from '../commands/update-function-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { getEscapeHatchCommands } from './escape-hatch-strategy'
import { flexReorderStrategy, getReorderIndex } from './flex-reorder-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { findReparentStrategy, getReparentTargetForFlexElement } from './reparent-strategy-helpers'
import {
  getDragTargets,
  getAbsoluteOffsetCommandsForSelectedElement,
} from './shared-absolute-move-strategy-helpers'

export const flexReparentToFlexStrategy: CanvasStrategy = {
  id: 'FLEX_REPARENT_TO_FLEX',
  name: 'Flex Reparent to Flex',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        canvasState.selectedElements[0],
        metadata,
      )
    } else {
      return false
    }
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
  fitness: (canvasState, interactionState, strategyState) => {
    if (
      flexReparentToFlexStrategy.isApplicable(
        canvasState,
        interactionState,
        strategyState.startingMetadata,
        strategyState.startingAllElementProps,
      ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      interactionState.interactionData.drag != null
    ) {
      const pointOnCanvas = offsetPoint(
        interactionState.interactionData.dragStart,
        interactionState.interactionData.drag,
      )

      const target = canvasState.selectedElements[0]

      const parentElementBounds = MetadataUtils.getParent(
        strategyState.startingMetadata,
        target,
      )?.globalFrame

      const isPointInParentBounds =
        parentElementBounds != null && rectContainsPoint(parentElementBounds, pointOnCanvas)

      const reparentStrategy = findReparentStrategy(canvasState, interactionState, strategyState)

      if (!isPointInParentBounds && reparentStrategy === 'FLEX_REPARENT_TO_FLEX') {
        return 2 // 2 here to beat flexReorderStrategy
      }
    }
    return 0 // fix fallback return 0
  },
  apply: (canvasState, interactionSession, strategyState) => {
    if (
      interactionSession.interactionData.type == 'DRAG' &&
      interactionSession.interactionData.drag != null
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
      const reparentResult = getReparentTargetForFlexElement(
        filteredSelectedElements,
        interactionSession,
        canvasState,
        strategyState,
      )

      if (
        reparentResult.shouldReparent &&
        reparentResult.newParent != null &&
        filteredSelectedElements.length === 1
      ) {
        const target = filteredSelectedElements[0]
        const newParent = reparentResult.newParent
        // Reparent the element.
        const newPath = EP.appendToPath(reparentResult.newParent, EP.toUid(target))
        const reparentCommand = reparentElement('permanent', target, reparentResult.newParent)

        const commandsBeforeReorder = [reparentCommand, updateSelectedViews('permanent', [newPath])]

        const commandsAfterReorder = [
          setElementsToRerenderCommand([newPath]),
          updateHighlightedViews('transient', []),
          setCursorCommand('transient', CSSCursor.Move),
        ]

        let commands: Array<CanvasCommand>
        if (reparentResult.shouldReorder) {
          // Reorder the newly reparented element into the flex ordering.
          const pointOnCanvas = offsetPoint(
            interactionSession.interactionData.dragStart,
            interactionSession.interactionData.drag,
          )

          const siblingsOfTarget = MetadataUtils.getChildrenPaths(
            strategyState.startingMetadata,
            newParent,
          )

          const newIndex = getReorderIndex(
            strategyState.startingMetadata,
            siblingsOfTarget,
            pointOnCanvas,
          )
          commands = [
            ...commandsBeforeReorder,
            reorderElement('permanent', newPath, newIndex),
            ...commandsAfterReorder,
          ]
        } else {
          commands = [...commandsBeforeReorder, ...commandsAfterReorder]
        }

        return {
          commands: commands,
          customState: strategyState.customStrategyState,
        }
      }
    }
    return emptyStrategyApplicationResult
  },
}
