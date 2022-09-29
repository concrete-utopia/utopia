import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { rectContainsPoint, CanvasVector, mod } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { AllElementProps } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { DeleteProperties, deleteProperties } from '../commands/delete-properties-command'
import { SetProperty, setProperty } from '../commands/set-property-command'

export function isValidFlowReorderTarget(elementMetadata: ElementInstanceMetadata | null): boolean {
  if (MetadataUtils.isPositionAbsolute(elementMetadata)) {
    return false
  } else if (elementMetadata?.specialSizeMeasurements.float !== 'none') {
    return false
  } else if (MetadataUtils.isPositionRelative(elementMetadata) && elementMetadata != null) {
    return !elementMetadata.specialSizeMeasurements.hasPositionOffset
  } else {
    return true
  }
}

function findSiblingIndexUnderPoint(
  point: CanvasVector,
  target: ElementPath | null,
  siblings: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): { newIndex: number; targetSiblingUnderMouse: ElementPath | null } {
  const newIndex = siblings.findIndex((sibling) => {
    const siblingMetadata = MetadataUtils.findElementByElementPath(metadata, sibling)
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    return (
      isValidFlowReorderTarget(siblingMetadata) &&
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
  target: ElementPath,
  targetSibling: ElementPath,
) {
  const displayType = MetadataUtils.findElementByElementPath(metadata, targetSibling)
    ?.specialSizeMeasurements.display
  const displayBlockInlineBlock =
    displayType === 'block' || displayType === 'inline-block' ? displayType : undefined

  const element = MetadataUtils.findElementByElementPath(metadata, target)
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
  allElementProps: AllElementProps,
  point: CanvasVector,
  target: ElementPath | null,
): {
  newIndex: number
  targetSiblingUnderMouse: ElementPath | null
} {
  if (target === null) {
    return {
      newIndex: -1,
      targetSiblingUnderMouse: null,
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
    allElementProps,
  )
  if (reorderResult.newIndex === -1) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
      targetSiblingUnderMouse: null,
    }
  } else {
    return {
      newIndex: reorderResult.newIndex,
      targetSiblingUnderMouse: reorderResult.targetSiblingUnderMouse,
    }
  }
}

const StyleDisplayProp = stylePropPathMappingFn('display', ['style'])
export function getOptionalDisplayPropCommands(
  target: ElementPath,
  newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null,
): Array<SetProperty | DeleteProperties> {
  if (newDisplayType?.type === 'add') {
    return [setProperty('always', target, StyleDisplayProp, newDisplayType.display)]
  } else if (newDisplayType?.type === 'remove') {
    return [deleteProperties('always', target, [StyleDisplayProp])]
  } else {
    return []
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
