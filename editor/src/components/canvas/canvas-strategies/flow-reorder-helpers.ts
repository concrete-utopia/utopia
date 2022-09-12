import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  CanvasVector,
  distance as euclideanDistance,
  getRectCenter,
  offsetPoint,
  pointIsClockwiseFromLine,
  rectContainsPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { AllElementProps, ElementProps } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { DeleteProperties, deleteProperties } from '../commands/delete-properties-command'
import { SetProperty, setProperty } from '../commands/set-property-command'

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
  siblingPath: ElementPath
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

function getSiblingDisplayValue(
  newIndex: number,
  siblings: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
) {
  const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, siblings[newIndex])
  const displayValue = siblingMetadata?.specialSizeMeasurements.display
  if (displayValue === 'inline-block' || displayValue === 'block') {
    return displayValue
  } else {
    return null
  }
}

function getSiblingDisplayValues(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
): Array<string | null> {
  return siblings.map((sibling) => {
    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    return siblingMetadata?.specialSizeMeasurements.display ?? null
  })
}

function isValidSibling(
  targetElementMetadata: ElementInstanceMetadata | null,
  siblingMetadata: ElementInstanceMetadata | null,
  displayTypeFiltering: 'same-display-type-only' | 'allow-mixed-display-type',
): boolean {
  const targetDisplayType = targetElementMetadata?.specialSizeMeasurements.display
  const siblingDisplayType = siblingMetadata?.specialSizeMeasurements.display

  return (
    displayTypeFiltering === 'allow-mixed-display-type' || siblingDisplayType === targetDisplayType
  )
}

function findClosestSibling(
  point: CanvasVector,
  target: ElementPath | null,
  siblings: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  displayTypeFiltering:
    | 'same-display-type-only'
    | 'allow-mixed-display-type' = 'allow-mixed-display-type',
): ReorderElement | null {
  let reorderResult: ReorderElement | null = null
  const targetElementMetadata = MetadataUtils.findElementByElementPath(metadata, target)

  for (const [index, sibling] of siblings.entries()) {
    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    if (
      isValidSibling(targetElementMetadata, siblingMetadata, displayTypeFiltering) &&
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
          siblingPath: sibling,
          siblingIndex: index,
          direction: flowDirectionForDisplayValue(siblingMetadata.specialSizeMeasurements.display),
        }
      }
    }
  }
  return reorderResult
}

function displayTypeBeforeIndex(
  displayValues: Array<string | null>,
  index: number,
): 'block' | 'inline-block' | undefined {
  const prevSiblingIndex = index

  const displayTypeOfPrevSibling = displayValues[prevSiblingIndex]
  const displayTypeOfNextSibling = displayValues[prevSiblingIndex + 1]

  if (displayTypeOfPrevSibling === 'inline-block' && displayTypeOfNextSibling === 'inline-block') {
    return 'inline-block'
  } else if (displayTypeOfPrevSibling === 'block' && displayTypeOfNextSibling === 'block') {
    return 'block'
  } else {
    return undefined
  }
}

function findNewIndexAndDisplayType(
  point: CanvasVector,
  target: ElementPath | null,
  siblings: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  reorderResult: ReorderElement,
  displayValues: Array<string | null>,
): { newIndex: number; newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null } {
  // Check which "side" of the target this falls on.
  const originalIndex = siblings.findIndex((path) => EP.pathsEqual(path, target))
  const newTargetIndex = reorderResult.siblingIndex
  const insertForward = newTargetIndex > originalIndex

  const targetElementMetadata = MetadataUtils.findElementByElementPath(metadata, target)
  const originalDisplayType = targetElementMetadata?.specialSizeMeasurements.display ?? 'block'

  let newIndex = insertForward ? newTargetIndex - 1 : newTargetIndex
  const directionForElement = flowDirectionForDisplayValue(originalDisplayType)
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

  return {
    newIndex: newIndex,
    newDisplayType: getNewDisplayType(
      targetElementMetadata,
      displayTypeBeforeIndex(displayValues, insertForward ? newIndex : newIndex - 1),
    ),
  }
}

function shouldRemoveDisplayProp(
  element: ElementInstanceMetadata | null,
  newDisplayValue: 'block' | 'inline-block' | undefined,
): boolean {
  if (element == null || element.specialSizeMeasurements.htmlElementName == null) {
    return false
  } else {
    return (
      // TODO global css overrides can change these defaults
      defaultDisplayTypeForHTMLElement(element.specialSizeMeasurements.htmlElementName) ===
      newDisplayValue
    )
  }
}

function getNewDisplayType(
  element: ElementInstanceMetadata | null,
  newDisplayValue: 'block' | 'inline-block' | undefined | null,
): AddDisplayBlockOrOnline | RemoveDisplayProp | null {
  if (shouldRemoveDisplayProp(element, newDisplayValue)) {
    return removeDisplayProp()
  } else if (newDisplayValue != null) {
    return addDisplayProp(newDisplayValue)
  } else {
    return null
  }
}

interface AddDisplayBlockOrOnline {
  type: 'add'
  display: 'block' | 'inline-block'
}

export function addDisplayProp(display: 'inline-block' | 'block'): AddDisplayBlockOrOnline {
  return {
    type: 'add',
    display: display,
  }
}

interface RemoveDisplayProp {
  type: 'remove'
}

export function removeDisplayProp(): RemoveDisplayProp {
  return { type: 'remove' }
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
  newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null
} {
  if (target === null) {
    return {
      newIndex: -1,
      newDisplayType: null,
    }
  }

  const displayValues = getSiblingDisplayValues(metadata, siblings)
  const reorderResult = findClosestSibling(
    point,
    target,
    siblings,
    metadata,
    allElementProps,
    displayTypeFiltering,
  )

  if (reorderResult == null) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
      newDisplayType: null,
    }
  } else if (EP.pathsEqual(reorderResult.siblingPath, target)) {
    // Reparenting to the same position that the existing element started in.
    return {
      newIndex: reorderResult.siblingIndex,
      newDisplayType: null,
    }
  } else {
    // Convert display type, maybe shift index
    const { newIndex, newDisplayType } = findNewIndexAndDisplayType(
      point,
      target,
      siblings,
      metadata,
      reorderResult,
      displayValues,
    )

    return {
      newIndex: newIndex,
      newDisplayType: newDisplayType,
    }
  }
}

const StyleDisplayProp = stylePropPathMappingFn('display', ['style'])
export function getOptionalDisplayPropCommands(
  target: ElementPath,
  newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null,
  withAutoConversion: 'with-auto-conversion' | 'no-conversion',
  whenToRun: 'always' | 'on-complete' = 'always',
): Array<SetProperty | DeleteProperties> {
  if (withAutoConversion === 'no-conversion') {
    return []
  } else {
    if (newDisplayType?.type === 'add') {
      return [setProperty(whenToRun, target, StyleDisplayProp, newDisplayType.display)]
    } else if (newDisplayType?.type === 'remove') {
      return [deleteProperties(whenToRun, target, [StyleDisplayProp])]
    } else {
      return []
    }
  }
}

export function findSiblingIndexUnderPoint(
  point: CanvasVector,
  target: ElementPath | null,
  siblings: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  displayTypeFiltering:
    | 'same-display-type-only'
    | 'allow-mixed-display-type' = 'allow-mixed-display-type',
): number {
  const targetElementMetadata = MetadataUtils.findElementByElementPath(metadata, target)

  return siblings.findIndex((sibling) => {
    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    return (
      isValidSibling(targetElementMetadata, siblingMetadata, displayTypeFiltering) &&
      frame != null &&
      rectContainsPoint(frame, point) &&
      MetadataUtils.isPositionedByFlow(siblingMetadata) &&
      !EP.pathsEqual(target, sibling)
    )
  })
}

export function getNewIndexAndInsertLine(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  point: CanvasVector,
  target: ElementPath | null,
  canvasScale: number,
): {
  newIndex: number
  newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null
  targetLineBeforeSibling: CanvasRectangle | null
} {
  const ReorderIndicatorSize = 2 / canvasScale
  const newIndex = findSiblingIndexUnderPoint(point, target, siblings, metadata)
  if (newIndex === -1) {
    return {
      newIndex: -1,
      newDisplayType: null,
      targetLineBeforeSibling: null,
    }
  }
  const siblingDisplayValue = getSiblingDisplayValue(newIndex, siblings, metadata)

  const targetLineBeforeSibling: CanvasRectangle = getInsertLineNextToSibling(
    MetadataUtils.getFrameInCanvasCoords(siblings[newIndex], metadata),
    siblingDisplayValue,
    ReorderIndicatorSize,
  )

  return {
    newIndex: newIndex,
    newDisplayType: getNewDisplayType(
      MetadataUtils.findElementByElementPath(metadata, target),
      siblingDisplayValue,
    ),
    targetLineBeforeSibling: targetLineBeforeSibling,
  }
}

export function getInsertLineNextToSibling(
  siblingFrame: CanvasRectangle | null,
  direction: 'inline-block' | 'block' | null,
  FlexReparentIndicatorSize: number,
): CanvasRectangle {
  if (siblingFrame == null) {
    return zeroCanvasRect
  } else {
    return direction === 'inline-block'
      ? canvasRectangle({
          x: siblingFrame.x,
          y: siblingFrame.y,
          height: siblingFrame.height,
          width: FlexReparentIndicatorSize,
        })
      : canvasRectangle({
          x: siblingFrame.x,
          y: siblingFrame.y,
          width: siblingFrame.width,
          height: FlexReparentIndicatorSize,
        })
  }
}
