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
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { getRectCenter, distance as euclideanDistance } from '../../../core/shared/math-utils'
import { AllElementProps, ElementProps } from '../../editor/store/editor-state'
import { absolute } from '../../../utils/utils'
import { FlowPositionMarker } from '../controls/flow-position-marker'
import { convertInlineBlock } from '../commands/convert-inline-block-command'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'

export const flowBlockReorderStategy: CanvasStrategy = {
  id: 'FLOW_BLOCK_REORDER',
  name: 'Reorder(Block)',
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
    {
      control: FlowPositionMarker,
      key: 'flow-position-marker-control',
      show: 'visible-only-while-active',
    },
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    if (
      flowBlockReorderStategy.isApplicable(
        canvasState,
        interactionState,
        strategyState.startingMetadata,
        strategyState.startingAllElementProps,
      ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
    ) {
      const target = canvasState.selectedElements[0]
      if (
        MetadataUtils.findElementByElementPath(strategyState.startingMetadata, target)
          ?.specialSizeMeasurements.display === 'block'
      ) {
        return 5
      }
      return 1
    } else {
      return 0
    }
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

      const reorderResult = getReorderIndex(
        strategyState.startingMetadata,
        siblingsOfTarget,
        pointOnCanvas,
        target,
        interactionState.allElementProps,
      )

      const { newIndex } = reorderResult

      const realNewIndex = newIndex > -1 ? newIndex : lastReorderIdx

      if (realNewIndex === unpatchedIndex) {
        return {
          commands: [
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
            convertInlineBlock('always', target, 'block'),
          ],
          customState: {
            ...strategyState.customStrategyState,
            lastReorderIdx: realNewIndex,
          },
        }
      } else {
        return {
          commands: [
            reorderElement('always', target, absolute(realNewIndex)),
            setElementsToRerenderCommand([target]),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
            convertInlineBlock('always', target, 'block'),
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

type FlowDirection = 'vertical' | 'horizontal'

function flowDirectionForDisplayValue(displayValue: string): FlowDirection {
  if (displayValue === 'inline' || displayValue === 'inline-block') {
    return 'horizontal'
  } else {
    return 'vertical'
  }
}

interface ReorderElement {
  distance: number
  centerPoint: CanvasPoint
  closestSibling: ElementPath
  siblingIndex: number
  direction: FlowDirection
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

function getReorderIndex(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  point: CanvasVector,
  existingElement: ElementPath | null,
  allElementProps: AllElementProps,
): {
  newIndex: number
} {
  if (existingElement === null) {
    return {
      newIndex: -1,
    }
  }

  let reorderResult: ReorderElement | null = null
  let siblingIndex: number = 0
  let displayValues: Array<string> = []

  // TODO stick elements?
  // TODO float?
  // TODO wrapping

  const existingElementMetadata = MetadataUtils.findElementByElementPath(metadata, existingElement)

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
          direction: flowDirectionForDisplayValue(siblingMetadata.specialSizeMeasurements.display),
        }
      }
    }
    displayValues.push(siblingMetadata?.specialSizeMeasurements.display || 'block')
    siblingIndex++
  }

  if (reorderResult == null) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
    }
  } else if (EP.pathsEqual(reorderResult.closestSibling, existingElement)) {
    // Reparenting to the same position that the existing element started in.
    return {
      newIndex: reorderResult.siblingIndex,
    }
  } else {
    // Check which "side" of the target this falls on.
    let newIndex = reorderResult.siblingIndex
    if (reorderResult.direction === 'vertical' && point.y > reorderResult.centerPoint.y) {
      newIndex++
    } else if (reorderResult.direction === 'horizontal' && point.x > reorderResult.centerPoint.x) {
      newIndex++
    }

    return { newIndex }
  }
}
