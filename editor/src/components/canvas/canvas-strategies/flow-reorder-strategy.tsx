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
  pointIsClockwiseFromLine,
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
import { FlowPositionMarker, FlowStartingPositionMarker } from '../controls/flow-position-marker'
import { convertInlineBlock } from '../commands/convert-inline-block-command'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'

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
    {
      control: FlowPositionMarker,
      key: 'flow-position-marker-control',
      show: 'visible-only-while-active',
    },
    {
      control: FlowStartingPositionMarker,
      key: 'flow-starting-position-marker-control',
      show: 'visible-only-while-active',
    },
    {
      control: DragOutlineControl,
      key: 'drag-outline-control',
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

      const pointOnCanvas = rawPointOnCanvas

      const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
      const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

      const reorderResult = getReorderIndex(
        strategyState.startingMetadata,
        siblingsOfTarget,
        pointOnCanvas,
        target,
        interactionState.allElementProps,
      )

      const { newIndex, newDisplayType } = reorderResult

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
        const newDisplayTypeCommands =
          newDisplayType == null ? [] : [convertInlineBlock('always', target, newDisplayType)]

        return {
          commands: [
            reorderElement('always', target, absolute(realNewIndex)),
            setElementsToRerenderCommand([target]),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
            ...newDisplayTypeCommands,
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
  bottomLeft: CanvasPoint
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
  newDisplayType?: 'block' | 'inline-block'
} {
  if (existingElement === null) {
    return {
      newIndex: -1,
    }
  }

  let reorderResult: ReorderElement | null = null
  let existingIndex: number = -1
  let workingIndex: number = 0
  let displayValues: Array<string> = []

  // TODO stick elements?
  // TODO float?
  // TODO wrapping

  const existingElementMetadata = MetadataUtils.findElementByElementPath(metadata, existingElement)

  for (const sibling of siblings) {
    if (EP.pathsEqual(sibling, existingElement)) {
      existingIndex = workingIndex
    }

    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    displayValues.push(siblingMetadata?.specialSizeMeasurements.display || 'block')
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    if (
      frame != null &&
      siblingMetadata != null &&
      MetadataUtils.isPositionedByFlow(siblingMetadata)
    ) {
      const siblingProps = allElementProps[EP.toString(sibling)] ?? {}
      const centerPoint = getCenterPositionInFlow(frame, siblingMetadata, siblingProps)
      const bottomLeft = offsetPoint(centerPoint, {
        x: -frame.width / 2,
        y: frame.height / 2,
      } as CanvasPoint)
      const distance = euclideanDistance(point, centerPoint)
      // First one that has been found or if it's closer than a previously found entry.
      if (reorderResult == null || distance < reorderResult.distance) {
        reorderResult = {
          distance: distance,
          centerPoint: centerPoint,
          bottomLeft: bottomLeft,
          closestSibling: sibling,
          siblingIndex: workingIndex,
          direction: flowDirectionForDisplayValue(siblingMetadata.specialSizeMeasurements.display),
        }
      }
    }
    workingIndex++
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
    const siblingIndex = reorderResult.siblingIndex
    const movedForward = siblingIndex > existingIndex

    const displayTypeBeforeIndex = (i: number): 'block' | 'inline-block' | undefined => {
      const prevSiblingIndex = movedForward ? i : i - 1

      const displayTypeOfPrevSibling = displayValues[prevSiblingIndex]
      const displayTypeOfNextSibling = displayValues[prevSiblingIndex + 1]

      if (
        displayTypeOfPrevSibling === 'inline-block' &&
        displayTypeOfNextSibling === 'inline-block'
      ) {
        return 'inline-block'
      } else if (displayTypeOfPrevSibling === 'block' && displayTypeOfNextSibling === 'block') {
        return 'block'
      } else {
        return undefined
      }
    }

    let newIndex = movedForward ? siblingIndex - 1 : siblingIndex

    const displayTypeForElementAtStart =
      displayTypeBeforeIndex(newIndex) ??
      existingElementMetadata?.specialSizeMeasurements.display ??
      'block'
    const directionForElement = flowDirectionForDisplayValue(displayTypeForElementAtStart)

    if (directionForElement !== reorderResult.direction) {
      // The directions don't match up, so check both the x and y based on a diagonal through the element
      if (pointIsClockwiseFromLine(point, reorderResult.bottomLeft, reorderResult.centerPoint)) {
        newIndex++
      }
    } else if (reorderResult.direction === 'vertical' && point.y > reorderResult.centerPoint.y) {
      newIndex++
    } else if (reorderResult.direction === 'horizontal' && point.x > reorderResult.centerPoint.x) {
      newIndex++
    }

    const newDisplayType = displayTypeBeforeIndex(newIndex)

    // if (newDisplayType === existingElementMetadata?.specialSizeMeasurements.display) {
    //   newDisplayType = undefined
    // }

    return {
      newIndex: newIndex,
      newDisplayType: newDisplayType,
    }
  }
}
