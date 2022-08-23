import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  offsetPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { getRectCenter, distance as euclideanDistance } from '../../../core/shared/math-utils'
import { AllElementProps, ElementProps } from '../../editor/store/editor-state'

export const flowReorderStategy: CanvasStrategy = {
  id: 'FLOW_REORDER',
  name: 'Flow Reorder',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length == 1) {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )
      return MetadataUtils.isPositionedByFlow(elementMetadata)
    } else {
      return false
    }
  },
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
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    return flowReorderStategy.isApplicable(
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

      const rawPointOnCanvas = offsetPoint(
        interactionState.interactionData.dragStart,
        interactionState.interactionData.drag,
      )

      const targetMetadata = MetadataUtils.findElementByElementPath(
        interactionState.metadata,
        target,
      )
      const targetProps = interactionState.allElementProps[EP.toString(target)] ?? {}
      const relativeOffset = getRelativeOffset(targetMetadata, targetProps)

      const pointOnCanvas = offsetPoint(rawPointOnCanvas, relativeOffset)

      const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
      const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

      const newIndex = getReorderIndex(
        strategyState.startingMetadata,
        siblingsOfTarget,
        pointOnCanvas,
        target,
        interactionState.allElementProps,
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

interface ReorderElement {
  distance: number
  centerPoint: CanvasPoint
  closestSibling: ElementPath
  siblingIndex: number
}

function getRelativeOffset(
  element: ElementInstanceMetadata | null,
  elementProps: ElementProps,
): CanvasPoint {
  if (element?.specialSizeMeasurements.position === 'relative') {
    const { left, right, top, bottom } = elementProps?.style
    const horizontalOffset = left ? -left : right ?? 0
    const verticalOffset = top ? -top : bottom ?? 0
    return { x: horizontalOffset, y: verticalOffset } as CanvasPoint
  } else {
    return { x: 0, y: 0 } as CanvasPoint
  }
}

function getCenterPositionInFlow(
  frame: CanvasRectangle,
  element: ElementInstanceMetadata,
  elementProps: ElementProps,
): CanvasPoint {
  const rawCenter = getRectCenter(frame)
  const relativeOffset = getRelativeOffset(element, elementProps)
  return offsetPoint(rawCenter, relativeOffset)
}

export function getReorderIndex(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  point: CanvasVector,
  existingElement: ElementPath | null,
  allElementProps: AllElementProps,
): number {
  let reorderResult: ReorderElement | null = null
  let siblingIndex: number = 0

  // TODO For relative elements we need to negatively offset them based on their top / bottom and left / right
  // TODO For sticky elements we need to do that plus... err... how the hell do we figure out how much to offset based on the scroll?
  // TODO What are all of the ways of achieving a horizontal layout in flow? float, display: inline-block, inline (when there is content), ...
  // TODO How should wrapping be handled here?

  for (const sibling of siblings) {
    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    if (
      frame != null &&
      siblingMetadata != null &&
      MetadataUtils.isPositionedByFlow(siblingMetadata)
    ) {
      const siblingProps = allElementProps[EP.toString(sibling)] ?? {}
      const centerPoint = getCenterPositionInFlow(frame, siblingMetadata, siblingProps)
      const distance = euclideanDistance(point, centerPoint)
      // First one that has been found or if it's closer than a previously found entry.
      if (reorderResult == null || distance < reorderResult.distance) {
        reorderResult = {
          distance: distance,
          centerPoint: centerPoint,
          closestSibling: sibling,
          siblingIndex: siblingIndex,
        }
      }
    }
    siblingIndex++
  }

  if (reorderResult == null) {
    // We were unable to find an appropriate entry.
    return -1
  } else if (EP.pathsEqual(reorderResult.closestSibling, existingElement)) {
    // Reparenting to the same position that the existing element started in.
    return reorderResult.siblingIndex
  } else {
    // Check which "side" of the target this falls on.
    let newIndex = reorderResult.siblingIndex
    if (point.y > reorderResult.centerPoint.y) {
      newIndex++
    }
    return newIndex
  }
}
