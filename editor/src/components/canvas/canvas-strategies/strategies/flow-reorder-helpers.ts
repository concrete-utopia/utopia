import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../../core/shared/dom-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import { CanvasRectangle, CanvasVector, mod } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { DeleteProperties, deleteProperties } from '../../commands/delete-properties-command'
import { SetProperty, setProperty } from '../../commands/set-property-command'
import { getTargetPathsFromInteractionTarget, InteractionTarget } from '../canvas-strategy-types'

export function isValidFlowReorderTarget(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
  if (MetadataUtils.isPositionAbsolute(elementMetadata)) {
    return false
  } else if (elementMetadata?.specialSizeMeasurements.hasTransform) {
    return false
  } else if (elementMetadata?.specialSizeMeasurements.float !== 'none') {
    return false
  } else {
    return MetadataUtils.isPositionedByFlow(elementMetadata)
  }
}

export function areAllSiblingsInOneDimensionFlexOrFlow(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const targetElement = MetadataUtils.findElementByElementPath(metadata, target)
  const siblings = MetadataUtils.getSiblings(metadata, target) // including target
  if (targetElement == null || siblings.length === 1) {
    return false
  }

  if (MetadataUtils.isParentYogaLayoutedContainerForElement(targetElement)) {
    const flexDirection = targetElement.specialSizeMeasurements.parentFlexDirection
    const targetDirection =
      flexDirection === 'row' || flexDirection === 'row-reverse' ? 'horizontal' : 'vertical'
    const shouldReverse = flexDirection?.includes('reverse') ?? false
    const frames = mapDropNulls((sibling) => {
      if (
        MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          sibling.elementPath,
          metadata,
        )
      ) {
        return sibling.globalFrame
      } else {
        return null
      }
    }, siblings)

    return areNonWrappingSiblings(frames, targetDirection, shouldReverse)
  } else {
    const targetDirection = getElementDirection(targetElement)
    const shouldReverse =
      targetDirection === 'horizontal' &&
      targetElement.specialSizeMeasurements?.textDirection === 'rtl'

    let allHorizontalOrVertical = true
    let frames: Array<CanvasRectangle> = []
    fastForEach(siblings, (sibling) => {
      if (isValidFlowReorderTarget(sibling.elementPath, metadata)) {
        if (getElementDirection(sibling) !== targetDirection) {
          allHorizontalOrVertical = false
        }
        if (sibling.globalFrame != null) {
          frames.push(sibling.globalFrame)
        }
      }
    })

    return allHorizontalOrVertical && areNonWrappingSiblings(frames, targetDirection, shouldReverse)
  }
}

function areNonWrappingSiblings(
  frames: Array<CanvasRectangle>,
  layoutDirection: 'horizontal' | 'vertical',
  shouldReverse: boolean,
): boolean {
  const orderedFrames = shouldReverse ? [...frames].reverse() : frames
  return orderedFrames.every((frame, i) => {
    if (i === 0) {
      return true
    } else {
      const prevFrame: CanvasRectangle = orderedFrames[i - 1]
      if (layoutDirection === 'horizontal') {
        // all elements are on the right side of the previous sibling
        return frame.x > prevFrame.x
      } else {
        // all elements are below of the previous sibling
        return frame.y > prevFrame.y
      }
    }
  })
}

export function getElementDirection(
  element: ElementInstanceMetadata | null,
): 'vertical' | 'horizontal' {
  const displayValue = element?.specialSizeMeasurements.display
  return displayValue?.includes('inline') ? 'horizontal' : 'vertical'
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

export function getOptionalDisplayPropCommandsForFlow(
  lastReorderIdx: number | null | undefined,
  interactionTarget: InteractionTarget,
  startingMetadata: ElementInstanceMetadataMap,
): Array<SetProperty | DeleteProperties> {
  const selectedElements = getTargetPathsFromInteractionTarget(interactionTarget)
  const target = selectedElements[0]
  const elementMetadata = MetadataUtils.findElementByElementPath(startingMetadata, target)
  if (!MetadataUtils.isPositionedByFlow(elementMetadata)) {
    return []
  }

  const siblingsOfTarget = MetadataUtils.getSiblingsProjectContentsOrdered(
    startingMetadata,
    target,
  ).map((element) => element.elementPath)
  const element = MetadataUtils.findElementByElementPath(startingMetadata, target)
  if (
    element != null &&
    lastReorderIdx != null &&
    lastReorderIdx !== siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
  ) {
    const newDisplayType = getNewDisplayTypeForIndex(
      startingMetadata,
      siblingsOfTarget[lastReorderIdx],
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
