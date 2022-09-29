import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { CanvasVector, mod } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { DeleteProperties, deleteProperties } from '../commands/delete-properties-command'
import { SetProperty, setProperty } from '../commands/set-property-command'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionTarget,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'

export function isValidFlowReorderTarget(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
  if (MetadataUtils.isPositionAbsolute(elementMetadata)) {
    return false
  } else if (elementMetadata?.specialSizeMeasurements.float !== 'none') {
    return false
  } else if (MetadataUtils.isPositionRelative(elementMetadata) && elementMetadata != null) {
    return !elementMetadata.specialSizeMeasurements.hasPositionOffset
  } else {
    return MetadataUtils.isPositionedByFlow(elementMetadata)
  }
}

function getNewDisplayTypeForIndex(
  metadata: ElementInstanceMetadataMap,
  targetSibling: ElementPath,
) {
  const displayType = MetadataUtils.findElementByElementPath(metadata, targetSibling)
    ?.specialSizeMeasurements.display
  const displayBlockInlineBlock =
    displayType === 'block' || displayType === 'inline-block' ? displayType : null

  return displayBlockInlineBlock
}

function shouldRemoveDisplayProp(
  element: ElementInstanceMetadata | null,
  newDisplayValue: 'block' | 'inline-block' | null,
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

const StyleDisplayProp = stylePropPathMappingFn('display', ['style'])
function getNewDisplayTypeCommands(
  element: ElementInstanceMetadata,
  newDisplayValue: 'block' | 'inline-block' | null,
): Array<SetProperty | DeleteProperties> {
  if (shouldRemoveDisplayProp(element, newDisplayValue)) {
    return [deleteProperties('always', element.elementPath, [StyleDisplayProp])]
  } else if (newDisplayValue != null) {
    return [setProperty('always', element.elementPath, StyleDisplayProp, newDisplayValue)]
  } else {
    return []
  }
}

export function getOptionalDisplayPropCommands(
  newResultOrLastIndex: number | null | undefined,
  interactionTarget: InteractionTarget,
  startingMetadata: ElementInstanceMetadataMap,
): Array<SetProperty | DeleteProperties> {
  const selectedElements = getTargetPathsFromInteractionTarget(interactionTarget)
  const target = selectedElements[0]
  const siblingsOfTarget = MetadataUtils.getSiblingsProjectContentsOrdered(
    startingMetadata,
    target,
  ).map((element) => element.elementPath)
  const element = MetadataUtils.findElementByElementPath(startingMetadata, target)
  if (
    element != null &&
    newResultOrLastIndex != null &&
    newResultOrLastIndex !== siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
  ) {
    const newDisplayType = getNewDisplayTypeForIndex(
      startingMetadata,
      siblingsOfTarget[newResultOrLastIndex],
    )
    return getNewDisplayTypeCommands(element, newDisplayType)
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
