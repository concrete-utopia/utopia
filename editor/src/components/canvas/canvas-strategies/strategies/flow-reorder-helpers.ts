import {
  flexDirectionToFlexForwardsOrBackwards,
  flexDirectionToSimpleFlexDirection,
  FlexForwardsOrBackwards,
  SimpleFlexDirection,
} from '../../../../core/layout/layout-utils'
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

// maybe move this to reparent-strategy-helpers
export function staticContainerDirections(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
):
  | {
      direction: SimpleFlexDirection | null
      forwardsOrBackwards: FlexForwardsOrBackwards | null
    }
  | 'non-1d-static' {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
  if (elementMetadata == null) {
    return 'non-1d-static'
  }
  const isFlex = elementMetadata.specialSizeMeasurements.layoutSystemForChildren === 'flex'
  const isFlow = elementMetadata.specialSizeMeasurements.layoutSystemForChildren === 'flow'
  const children = MetadataUtils.getChildren(metadata, path)

  // TODO make it work if there is 0 children but the container is Flex!!!!!!!!!!!!

  if (!isFlex && !isFlow) {
    return 'non-1d-static'
  }

  if (isFlex) {
    // TODO check if 1D!
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return MetadataUtils.getSimpleFlexDirection(element)
  } else {
    const flowChildren = children.filter(
      (child) => child.specialSizeMeasurements.position !== 'absolute',
    )

    // TODO !!!!!!!!! this check should not be here!! we are mixing responsibilities!!!! the container dimensions don't depend on the number
    // we should probably (re) use the rules from flowParentAbsoluteOrStatic instead of putting rules here
    // for example, should only disallow a Flow parent if it has a single child which is contiguous with the padded area
    if (flowChildren.length < 2) {
      return 'non-1d-static'
    }

    const directionResult = areAllSiblingsInOneDimensionFlexOrFlow(
      children[0].elementPath,
      metadata,
    )

    return directionResult
  }
}

export function areAllSiblingsInOneDimensionFlexOrFlow(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
):
  | {
      direction: SimpleFlexDirection | null
      forwardsOrBackwards: FlexForwardsOrBackwards | null
    }
  | 'non-1d-static' {
  const targetElement = MetadataUtils.findElementByElementPath(metadata, target)
  const siblings = MetadataUtils.getSiblings(metadata, target) // including target
  if (targetElement == null || siblings.length === 1) {
    return 'non-1d-static'
  }

  if (MetadataUtils.isParentFlexLayoutedContainerForElement(targetElement)) {
    const parentElement = MetadataUtils.getParent(metadata, target)
    const flexDirection = MetadataUtils.getSimpleFlexDirection(parentElement)
    const targetDirection = flexDirection.direction === 'row' ? 'horizontal' : 'vertical' // TODO unify row and horizontal types

    const shouldReverse = flexDirection.forwardsOrBackwards === 'reverse'
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

    const is1D = areNonWrappingSiblings(frames, targetDirection, shouldReverse)
    if (!is1D) {
      return 'non-1d-static'
    }
    return flexDirection
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

    const is1D =
      allHorizontalOrVertical && areNonWrappingSiblings(frames, targetDirection, shouldReverse)
    if (!is1D) {
      return 'non-1d-static'
    }
    return {
      direction: targetDirection === 'horizontal' ? 'row' : 'column', // TODO use 'horizontal' | 'vertical' in the main type
      forwardsOrBackwards: shouldReverse ? 'reverse' : 'forward',
    }
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
