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
  distance as euclideanDistance,
  getRectCenter,
  offsetPoint,
  pointIsClockwiseFromLine,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { Utils } from '../../../uuiui-deps'
import { AllElementProps, ElementProps } from '../../editor/store/editor-state'

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

export function getFlowReorderIndex(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  point: CanvasVector,
  target: ElementPath | null,
  allElementProps: AllElementProps,
  displayTypeFiltering:
    | 'same-display-type-only'
    | 'allow-mixed-display-type' = 'allow-mixed-display-type',
): {
  newIndex: number
  newDisplayType?: 'block' | 'inline-block'
} {
  if (target === null) {
    return {
      newIndex: -1,
    }
  }

  let reorderResult: ReorderElement | null = null
  let displayValues: Array<string | undefined> = []

  // TODO stick elements?
  // TODO float?
  // TODO wrapping

  const targetElementMetadata = MetadataUtils.findElementByElementPath(metadata, target)
  const displayType = targetElementMetadata?.specialSizeMeasurements.display
  const targetIndex = siblings.findIndex((path) => EP.pathsEqual(path, target))

  for (const [index, sibling] of siblings.entries()) {
    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    const siblingDisplayType = siblingMetadata?.specialSizeMeasurements.display
    displayValues.push(siblingDisplayType)
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    const isValidSibling =
      displayTypeFiltering === 'allow-mixed-display-type' ?? siblingDisplayType === displayType
    if (
      isValidSibling &&
      frame != null &&
      siblingMetadata != null &&
      MetadataUtils.isPositionedByFlow(siblingMetadata)
    ) {
      const siblingProps = allElementProps[EP.toString(siblingMetadata.elementPath)] ?? {}
      const centerPoint = getCenterPositionInFlow(frame, siblingMetadata, siblingProps)
      const bottomLeft = offsetPoint(centerPoint, {
        x: -frame.width / 2,
        y: frame.height / 2,
      } as CanvasPoint)

      // First one that has been found or if it's closer than a previously found entry.
      const distance = euclideanDistance(point, centerPoint)
      if (reorderResult == null || distance < reorderResult.distance) {
        reorderResult = {
          distance: distance,
          centerPoint: centerPoint,
          bottomLeft: bottomLeft,
          closestSibling: sibling,
          siblingIndex: index,
          direction: flowDirectionForDisplayValue(siblingMetadata.specialSizeMeasurements.display),
        }
      }
    }
  }

  if (reorderResult == null) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
    }
  } else if (EP.pathsEqual(reorderResult.closestSibling, target)) {
    // Reparenting to the same position that the existing element started in.
    return {
      newIndex: reorderResult.siblingIndex,
    }
  } else {
    // Check which "side" of the target this falls on.
    const siblingIndex = reorderResult.siblingIndex
    const movedForward = siblingIndex > targetIndex

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
      targetElementMetadata?.specialSizeMeasurements.display ??
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

    return {
      newIndex: newIndex,
      newDisplayType: newDisplayType,
    }
  }
}
