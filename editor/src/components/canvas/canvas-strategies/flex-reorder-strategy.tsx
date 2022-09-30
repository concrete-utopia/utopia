import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasVector, offsetPoint, rectContainsPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { reorderElement } from '../commands/reorder-element-command'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import * as EP from '../../../core/shared/element-path'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { CSSCursor } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { ParentOutlines } from '../controls/parent-outlines'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { ParentBounds } from '../controls/parent-bounds'
import { absolute } from '../../../utils/utils'
import { isReorderAllowed } from './reorder-utils'

export const flexReorderStrategy: CanvasStrategy = {
  id: 'FLEX_REORDER',
  name: () => 'Reorder (Flex)',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        selectedElements[0],
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
      interactionState.startingMetadata,
      interactionState.startingAllElementProps,
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
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

      const target = selectedElements[0]
      const siblingsOfTarget = MetadataUtils.getSiblings(
        interactionState.startingMetadata,
        target,
      ).map((element) => element.elementPath)

      if (!isReorderAllowed(siblingsOfTarget)) {
        return strategyApplicationResult(
          [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
          {},
          'failure',
        )
      }

      const pointOnCanvas = offsetPoint(
        interactionState.interactionData.dragStart,
        interactionState.interactionData.drag,
      )

      const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
      const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

      const newIndex = reorderIndexForReorder(
        interactionState.startingMetadata,
        siblingsOfTarget,
        pointOnCanvas,
      )

      const realNewIndex = newIndex > -1 ? newIndex : lastReorderIdx

      if (realNewIndex === unpatchedIndex) {
        return strategyApplicationResult(
          [
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
          ],
          {
            lastReorderIdx: realNewIndex,
          },
        )
      } else {
        return strategyApplicationResult(
          [
            reorderElement('always', target, absolute(realNewIndex)),
            setElementsToRerenderCommand([target]),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
          ],
          {
            lastReorderIdx: realNewIndex,
          },
        )
      }
    } else {
      // Fallback for when the checks above are not satisfied.
      return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.Move)])
    }
  },
}

function reorderIndexForReorder(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  point: CanvasVector,
): number {
  const targetSiblingIdx = siblings.findIndex((sibling) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    return (
      frame != null &&
      rectContainsPoint(frame, point) &&
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(sibling, metadata)
    )
  })
  return targetSiblingIdx
}
