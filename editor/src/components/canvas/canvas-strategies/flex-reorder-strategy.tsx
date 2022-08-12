import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { offsetPoint, rectContainsPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { reorderElement } from '../commands/reorder-element-command'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import {
  CanvasPoint,
  canvasPoint,
  CanvasVector,
  CanvasRectangle,
} from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import { reverse, stripNulls } from '../../../core/shared/array-utils'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { CSSCursor } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { ParentOutlines } from '../controls/parent-outlines'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { ParentBounds } from '../controls/parent-bounds'
import { getReorderIndex } from './reparent-strategy-helpers'

export const flexReorderStrategy: CanvasStrategy = {
  id: 'FLEX_REORDER',
  name: 'Flex Reorder',
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
    return flexReorderStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type !== 'DRAG') {
      return emptyStrategyApplicationResult
    }

    if (interactionState.interactionData.drag != null) {
      const { selectedElements } = canvasState
      const target = selectedElements[0]

      const siblingsOfTarget = MetadataUtils.getSiblings(
        strategyState.startingMetadata,
        target,
      ).map((element) => element.elementPath)

      const pointOnCanvas = offsetPoint(
        interactionState.interactionData.dragStart,
        interactionState.interactionData.drag,
      )

      const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
      const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

      const newIndex = getReorderIndex(
        strategyState.startingMetadata,
        siblingsOfTarget,
        pointOnCanvas,
      )

      const realNewIndex = newIndex > -1 ? newIndex : lastReorderIdx

      if (realNewIndex === unpatchedIndex) {
        return {
          commands: [
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
          ],
          customState: {
            ...strategyState.customStrategyState,
            lastReorderIdx: realNewIndex,
          },
        }
      } else {
        return {
          commands: [
            reorderElement('always', target, realNewIndex),
            setElementsToRerenderCommand([target]),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
          ],
          customState: {
            ...strategyState.customStrategyState,
            lastReorderIdx: realNewIndex,
          },
        }
      }
    } else {
      // Fallback for when the checks above are not satisfied.
      return {
        commands: [setCursorCommand('mid-interaction', CSSCursor.Move)],
        customState: null,
      }
    }
  },
}
