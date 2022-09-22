import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { rectContainsPoint, CanvasVector, mod } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { DeleteProperties, deleteProperties } from '../commands/delete-properties-command'
import { SetProperty, setProperty } from '../commands/set-property-command'

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

function findSiblingIndexUnderPoint(
  point: CanvasVector,
  target: ElementPath | null,
  siblings: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  displayTypeFiltering:
    | 'same-display-type-only'
    | 'allow-mixed-display-type' = 'allow-mixed-display-type',
): { newIndex: number; targetSiblingUnderMouse: ElementPath | null } {
  const targetElementMetadata = MetadataUtils.findElementByElementPath(metadata, target)

  const newIndex = siblings.findIndex((sibling) => {
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
  return { newIndex: newIndex, targetSiblingUnderMouse: siblings[newIndex] }
}

export function getNewDisplayTypeForIndex(
  metadata: ElementInstanceMetadataMap,
  element: ElementInstanceMetadata | null,
  targetSibling: ElementPath,
) {
  const displayType = MetadataUtils.findElementByElementPath(metadata, targetSibling)
    ?.specialSizeMeasurements.display
  const displayBlockInlineBlock =
    displayType === 'block' || displayType === 'inline-block' ? displayType : undefined
  return getNewDisplayType(element, displayBlockInlineBlock)
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
  newDisplayValue: 'block' | 'inline-block' | undefined,
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
  latestMetadata: ElementInstanceMetadataMap,
  point: CanvasVector,
  target: ElementPath | null,
  displayTypeFiltering:
    | 'same-display-type-only'
    | 'allow-mixed-display-type' = 'allow-mixed-display-type',
): {
  newIndex: number
  targetSiblingUnderMouse: ElementPath | null
  newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null
} {
  if (target === null) {
    return {
      newIndex: -1,
      targetSiblingUnderMouse: null,
      newDisplayType: null,
    }
  }
  const siblings = MetadataUtils.getSiblingsProjectContentsOrdered(latestMetadata, target).map(
    (element) => element.elementPath,
  )

  const reorderResult = findSiblingIndexUnderPoint(
    point,
    target,
    siblings,
    latestMetadata,
    displayTypeFiltering,
  )
  if (reorderResult.newIndex === -1) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
      targetSiblingUnderMouse: null,
      newDisplayType: null,
    }
  } else {
    // Convert display type if needed
    const newDisplayType = getNewDisplayTypeForIndex(
      latestMetadata,
      MetadataUtils.findElementByElementPath(latestMetadata, target),
      siblings[reorderResult.newIndex],
    )

    return {
      newIndex: reorderResult.newIndex,
      targetSiblingUnderMouse: reorderResult.targetSiblingUnderMouse,
      newDisplayType: newDisplayType,
    }
  }
}

const StyleDisplayProp = stylePropPathMappingFn('display', ['style'])
export function getOptionalDisplayPropCommands(
  target: ElementPath,
  newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null,
  withAutoConversion: 'with-auto-conversion' | 'no-conversion',
): Array<SetProperty | DeleteProperties> {
  if (withAutoConversion === 'no-conversion') {
    return []
  } else {
    if (newDisplayType?.type === 'add') {
      return [setProperty('always', target, StyleDisplayProp, newDisplayType.display)]
    } else if (newDisplayType?.type === 'remove') {
      return [deleteProperties('always', target, [StyleDisplayProp])]
    } else {
      return []
    }
  }
}

export const ReorderChangeThreshold = 32
export function findNewIndex(
  startingIndex: number,
  drag: CanvasVector,
  siblings: Array<ElementPath>,
  shouldRound: 'rounded-value' | 'raw-value',
): number {
  const dragDelta = drag.x // TODO optional vertical direction
  const reorderIndexPosition = dragDelta / ReorderChangeThreshold
  const indexOffset =
    shouldRound === 'rounded-value' ? Math.round(reorderIndexPosition) : reorderIndexPosition
  return mod(startingIndex + indexOffset, siblings.length)
}
