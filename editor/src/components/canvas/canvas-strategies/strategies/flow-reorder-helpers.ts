import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { allElemsEqual, mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  DetectedLayoutSystem,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import {
  isInfinityRectangle,
  mod,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import type {
  Direction,
  ForwardOrReverse,
  SimpleFlexDirection,
} from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { DeleteProperties } from '../../commands/delete-properties-command'
import type { SetProperty } from '../../commands/set-property-command'
import { setProperty } from '../../commands/set-property-command'
import { getTargetPathsFromInteractionTarget, InteractionTarget } from '../canvas-strategy-types'
import { AllElementProps } from '../../../editor/store/editor-state'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'

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
  elementPathTree: ElementPathTrees,
): boolean {
  return (
    singleAxisAutoLayoutSiblingDirections(target, metadata, elementPathTree) !==
    'non-single-axis-autolayout'
  )
}

export function singleAxisAutoLayoutSiblingDirections(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): SingleAxisAutolayoutContainerDirections | 'non-single-axis-autolayout' {
  const siblings = MetadataUtils.getSiblingsParticipatingInAutolayoutOrdered(
    metadata,
    elementPathTree,
    target,
  ) // including target
  if (siblings.length === 1) {
    return 'non-single-axis-autolayout'
  }

  return singleAxisAutoLayoutChildrenDirections(siblings, metadata)
}

export type SingleAxisAutolayoutContainerDirections = {
  direction: Direction
  forwardOrReverse: ForwardOrReverse | null
  flexOrFlow: 'flex' | 'flow'
}

export function singleAxisAutoLayoutContainerDirections(
  container: ElementPath,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): SingleAxisAutolayoutContainerDirections | 'non-single-axis-autolayout' {
  const children = MetadataUtils.getOrderedChildrenParticipatingInAutoLayout(
    metadata,
    elementPathTree,
    container,
  )

  const layoutSystem = MetadataUtils.findLayoutSystemForChildren(
    metadata,
    elementPathTree,
    container,
  )
  const flexDirection =
    MetadataUtils.findFlexDirectionForChildren(metadata, elementPathTree, container) ?? 'row'

  return singleAxisAutoLayoutDirections(
    children,
    metadata,
    layoutSystem,
    MetadataUtils.flexDirectionToSimpleFlexDirection(flexDirection),
  )
}

export function singleAxisAutoLayoutChildrenDirections(
  children: ElementInstanceMetadata[],
  metadata: ElementInstanceMetadataMap,
): SingleAxisAutolayoutContainerDirections | 'non-single-axis-autolayout' {
  if (children.length < 1) {
    return 'non-single-axis-autolayout'
  }

  const layoutSystem = children[0].specialSizeMeasurements.parentLayoutSystem
  const flexDirection = MetadataUtils.flexDirectionToSimpleFlexDirection(
    children[0].specialSizeMeasurements.parentFlexDirection ?? 'row',
  )

  return singleAxisAutoLayoutDirections(children, metadata, layoutSystem, flexDirection)
}

export function singleAxisAutoLayoutDirections(
  children: ElementInstanceMetadata[],
  metadata: ElementInstanceMetadataMap,
  layoutSystem: DetectedLayoutSystem,
  flexDirection: SimpleFlexDirection,
): SingleAxisAutolayoutContainerDirections | 'non-single-axis-autolayout' {
  if (layoutSystem === 'flex') {
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

    let numberOfHorizontalElements = 0
    let numberOfVerticalElements = 0

    let childrenFrames: Array<CanvasRectangle> = []

    fastForEach(children, (child) => {
      const childFrame = zeroRectIfNullOrInfinity(child.globalFrame)
      if (childFrame.width > 0 && childFrame.height > 0) {
        childrenFrames.push(childFrame)

        if (getElementDirection(child) === 'horizontal') {
          numberOfHorizontalElements++
        } else {
          numberOfVerticalElements++
        }
      }
    })

    const predominantDirection =
      numberOfHorizontalElements > numberOfVerticalElements ? 'horizontal' : 'vertical'

    const shouldReverse =
      predominantDirection === 'horizontal' &&
      firstChild.specialSizeMeasurements?.parentTextDirection === 'rtl'

    const is1D =
      (numberOfHorizontalElements <= 1 || numberOfVerticalElements <= 1) &&
      areNonWrappingSiblings(childrenFrames, predominantDirection, shouldReverse)
    if (!is1D) {
      return 'non-single-axis-autolayout'
    }
    return {
      direction: predominantDirection,
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
