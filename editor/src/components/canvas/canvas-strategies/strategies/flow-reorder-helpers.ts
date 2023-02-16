import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { defaultDisplayTypeForHTMLElement } from '../../../../core/shared/dom-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import {
  CanvasRectangle,
  CanvasVector,
  isInfinityRectangle,
  mod,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import { Direction, ForwardOrReverse } from '../../../inspector/common/css-utils'
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
  const siblings = MetadataUtils.getSiblingsUnordered(metadata, target) // including target
  if (siblings.length === 1) {
    return false
  }

  return (
    singleAxisAutoLayoutContainerDirections(EP.parentPath(target), metadata) !==
    'non-single-axis-autolayout'
  )
}

export type SingleAxisAutolayoutContainerDirections = {
  direction: Direction | null
  forwardOrReverse: ForwardOrReverse | null
  flexOrFlow: 'flex' | 'flow'
}

export function singleAxisAutoLayoutContainerDirections(
  container: ElementPath,
  metadata: ElementInstanceMetadataMap,
): SingleAxisAutolayoutContainerDirections | 'non-single-axis-autolayout' {
  const containerElement = MetadataUtils.findElementByElementPath(metadata, container)
  const children = MetadataUtils.getOrderedChildrenParticipatingInAutoLayout(metadata, container)
  if (containerElement == null) {
    return 'non-single-axis-autolayout'
  }

  const layoutSystem = containerElement.specialSizeMeasurements.layoutSystemForChildren

  if (layoutSystem === 'flex') {
    const flexDirection = MetadataUtils.getSimpleFlexDirection(containerElement)
    const targetDirection = flexDirection.direction

    const shouldReverse = flexDirection.forwardOrReverse === 'reverse'
    const childrenFrames = mapDropNulls((child) => {
      if (
        MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          child.elementPath,
          metadata,
        )
      ) {
        return zeroRectIfNullOrInfinity(child.globalFrame)
      } else {
        return null
      }
    }, children)

    const is1D = areNonWrappingSiblings(childrenFrames, targetDirection, shouldReverse)
    if (!is1D) {
      return 'non-single-axis-autolayout'
    }
    return { ...flexDirection, flexOrFlow: 'flex' }
  } else if (layoutSystem === 'flow') {
    if (children.length === 0) {
      return {
        direction: 'vertical',
        forwardOrReverse: 'forward',
        flexOrFlow: 'flow',
      }
    }
    const firstChild = children[0]
    const targetDirection = getElementDirection(firstChild)
    const shouldReverse =
      targetDirection === 'horizontal' &&
      firstChild.specialSizeMeasurements?.parentTextDirection === 'rtl'

    let allHorizontalOrVertical = true
    let childrenFrames: Array<CanvasRectangle> = []

    // TODO turn this into a loop with early return
    fastForEach(children, (child) => {
      if (getElementDirection(child) !== targetDirection) {
        allHorizontalOrVertical = false
      }
      if (child.globalFrame != null) {
        const childFrame = isInfinityRectangle(child.globalFrame)
          ? zeroCanvasRect
          : child.globalFrame
        childrenFrames.push(childFrame)
      }
    })

    const is1D =
      allHorizontalOrVertical &&
      areNonWrappingSiblings(childrenFrames, targetDirection, shouldReverse)
    if (!is1D) {
      return 'non-single-axis-autolayout'
    }
    return {
      direction: targetDirection,
      forwardOrReverse: shouldReverse ? 'reverse' : 'forward',
      flexOrFlow: 'flow',
    }
  } else {
    return 'non-single-axis-autolayout'
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

export function getElementDirection(element: ElementInstanceMetadata | null): Direction {
  const displayValue = element?.specialSizeMeasurements.display
  return displayValue?.includes('inline') ? 'horizontal' : 'vertical'
}

const StyleDisplayProp = stylePropPathMappingFn('display', styleStringInArray)

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

  const siblingsOfTarget = MetadataUtils.getSiblingsOrdered(startingMetadata, target).map(
    (element) => element.elementPath,
  )
  const element = MetadataUtils.findElementByElementPath(startingMetadata, target)
  if (
    element != null &&
    lastReorderIdx != null &&
    lastReorderIdx !== siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
  ) {
    const targetSibling = MetadataUtils.findElementByElementPath(
      startingMetadata,
      siblingsOfTarget[lastReorderIdx],
    )
    const elementDisplayType = elementMetadata?.specialSizeMeasurements.display ?? null
    const newDirection = getElementDirection(targetSibling)
    return getOptionalCommandToConvertDisplayInlineBlock(target, elementDisplayType, newDirection)
  } else {
    return []
  }
}

export function getOptionalCommandToConvertDisplayInlineBlock(
  target: ElementPath,
  displayValue: string | null,
  convertTo: Direction | 'do-not-convert',
): Array<SetProperty> {
  switch (convertTo) {
    case 'horizontal':
      return getOptionalCommandToConvertDisplayInline(target, displayValue)
    case 'vertical':
      return getOptionalCommandToRemoveDisplayInline(target, displayValue)
    default:
      return []
  }
}

function getOptionalCommandToConvertDisplayInline(
  target: ElementPath,
  displayValue: string | null,
): Array<SetProperty> {
  const displayValueKnownGood = ['block', 'flex', 'grid'].some((p) => displayValue === p)

  if (displayValue == null) {
    return [setProperty('always', target, StyleDisplayProp, `inline-block`)]
  } else if (displayValueKnownGood) {
    return [setProperty('always', target, StyleDisplayProp, `inline-${displayValue}`)]
  } else {
    return []
  }
}

function getOptionalCommandToRemoveDisplayInline(
  target: ElementPath,
  displayValue: string | null,
): Array<SetProperty> {
  const displayValueKnownGood = ['inline-block', 'inline-flex', 'inline-grid'].some(
    (p) => displayValue === p,
  )

  if (displayValue == null || displayValue === 'inline') {
    return [setProperty('always', target, StyleDisplayProp, `block`)]
  } else if (displayValueKnownGood) {
    return [setProperty('always', target, StyleDisplayProp, displayValue.slice('inline-'.length))]
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
