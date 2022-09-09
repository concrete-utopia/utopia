import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { CanvasVector, rectContainsPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { DeleteProperties, deleteProperties } from '../commands/delete-properties-command'
import { SetProperty, setProperty } from '../commands/set-property-command'

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

function findSiblingIndexUnderPoint(
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

function displayTypeBeforeIndex(
  displayValues: Array<string | null>,
  index: number,
): 'block' | 'inline-block' | undefined {
  const prevSiblingIndex = index - 1

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
  target: ElementPath | null,
  metadata: ElementInstanceMetadataMap,
  newIndex: number,
  displayValues: Array<string | null>,
): { newIndex: number; newDisplayType: AddDisplayBlockOrOnline | RemoveDisplayProp | null } {
  const targetElementMetadata = MetadataUtils.findElementByElementPath(metadata, target)

  return {
    newIndex: newIndex,
    newDisplayType: getNewDisplayType(
      targetElementMetadata,
      displayTypeBeforeIndex(displayValues, newIndex),
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
  startingMetadata: ElementInstanceMetadataMap,
  latestMetadata: ElementInstanceMetadataMap,
  point: CanvasVector,
  target: ElementPath | null,
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

  // SIBLING INDEXES FROM STARTING METADATA
  // const siblings = MetadataUtils.getSiblings(startingMetadata, target).map(
  //   (element) => element.elementPath,

  // SIBLING INDEXES FROM FRESH METADATA
  const siblings = MetadataUtils.getSiblings(latestMetadata, target).map(
    (element) => element.elementPath,
  )
  const displayValues = getSiblingDisplayValues(latestMetadata, siblings)

  // FRAME RESULT FROM FRESH METADATA
  const reorderResult = findSiblingIndexUnderPoint(
    point,
    target,
    siblings,
    latestMetadata,
    displayTypeFiltering,
  )
  if (reorderResult === -1) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
      newDisplayType: null,
    }
  } else {
    // Convert display type if needed
    const { newIndex, newDisplayType } = findNewIndexAndDisplayType(
      target,
      latestMetadata,
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
