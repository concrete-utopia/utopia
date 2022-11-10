import { FlexForwardsOrBackwards, SimpleFlexDirection } from '../../../../core/layout/layout-utils'
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
  const siblings = MetadataUtils.getSiblings(metadata, target) // including target
  if (siblings.length === 1) {
    return false
  }

  return (
    singleAxisAutoLayoutContainerDirections(EP.parentPath(target), metadata) !==
    'non-single-axis-autolayout'
  )
}

export function singleAxisAutoLayoutContainerDirections(
  container: ElementPath,
  metadata: ElementInstanceMetadataMap,
):
  | {
      direction: SimpleFlexDirection | null
      forwardsOrBackwards: FlexForwardsOrBackwards | null
      flexOrFlow: 'flex' | 'flow'
    }
  | 'non-single-axis-autolayout' {
  const containerElement = MetadataUtils.findElementByElementPath(metadata, container)
  const children = MetadataUtils.getChildren(metadata, container)
  if (containerElement == null) {
    return 'non-single-axis-autolayout'
  }

  const layoutSystem = containerElement.specialSizeMeasurements.layoutSystemForChildren

  if (layoutSystem === 'flex') {
    const flexDirection = MetadataUtils.getSimpleFlexDirection(containerElement)
    const targetDirection = flexDirection.direction === 'row' ? 'horizontal' : 'vertical' // TODO unify row and horizontal types

    const shouldReverse = flexDirection.forwardsOrBackwards === 'reverse'
    const childrenFrames = mapDropNulls((child) => {
      if (
        MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          child.elementPath,
          metadata,
        )
      ) {
        return child.globalFrame
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
        direction: 'column',
        forwardsOrBackwards: 'forward',
        flexOrFlow: 'flow',
      }
    }
    const firstChild = children[0]
    const targetDirection = getElementDirection(firstChild)
    const shouldReverse =
      targetDirection === 'horizontal' &&
      firstChild.specialSizeMeasurements?.textDirection === 'rtl'

    let allHorizontalOrVertical = true
    let childrenFrames: Array<CanvasRectangle> = []

    // TODO turn this into a loop with early return
    fastForEach(children, (child) => {
      if (isValidFlowReorderTarget(child.elementPath, metadata)) {
        if (getElementDirection(child) !== targetDirection) {
          allHorizontalOrVertical = false
        }
        if (child.globalFrame != null) {
          childrenFrames.push(child.globalFrame)
        }
      }
    })

    const is1D =
      allHorizontalOrVertical &&
      areNonWrappingSiblings(childrenFrames, targetDirection, shouldReverse)
    if (!is1D) {
      return 'non-single-axis-autolayout'
    }
    return {
      direction: targetDirection === 'horizontal' ? 'row' : 'column', // TODO use 'horizontal' | 'vertical' in the main type
      forwardsOrBackwards: shouldReverse ? 'reverse' : 'forward',
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
    return getNewDisplayTypeCommands(element, newDisplayType) // TODO this is wrong, it will convert a display: flex to block!!!
  } else {
    return []
  }
}

export function getOptionalCommandToConvertDisplayInlineBlock(
  target: ElementPath,
  displayValue: string | null,
  convertTo: 'row' | 'column' | 'do-not-convert',
): Array<SetProperty> {
  switch (convertTo) {
    case 'row':
      return getOptionalCommandToConvertDisplayInline(target, displayValue)
    case 'column':
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
