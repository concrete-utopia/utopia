import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import {
  zeroCanvasPoint,
  isFiniteRectangle,
  isInfinityRectangle,
  zeroCanvasRect,
  CanvasPoint,
  LocalPoint,
  CanvasRectangle,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { AllElementProps } from '../../../editor/store/editor-state'
import { cssPixelLength } from '../../../inspector/common/css-utils'
import {
  nukeAllAbsolutePositioningPropsCommands,
  nukeSizingPropsForAxisCommand,
  setElementTopLeft,
} from '../../../inspector/inspector-common'
import { CanvasCommand } from '../../commands/commands'
import { setCssLengthProperty, setExplicitCssValue } from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import {
  replaceContentAffectingPathsWithTheirChildrenRecursive,
  getElementContentAffectingType,
} from './group-like-helpers'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'

export function isAbsolutePositionedFrame(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): boolean {
  return (
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(metadata, elementPath),
    ) &&
    MetadataUtils.getChildrenPathsUnordered(metadata, elementPath).length > 0 &&
    replaceContentAffectingPathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      MetadataUtils.getChildrenPathsUnordered(metadata, elementPath),
    ).every((childPath) =>
      MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(metadata, childPath)),
    )
  )
}

// when removing the parent from between the children and the grandparent
function offsetChildrenByVectorCommands(
  childInstances: Array<ElementInstanceMetadata>,
  offset: CanvasPoint | LocalPoint,
) {
  return childInstances.flatMap((child) =>
    setElementTopLeft(child, {
      top: child.specialSizeMeasurements.offset.y + offset.y,
      left: child.specialSizeMeasurements.offset.x + offset.x,
    }),
  )
}

// when putting new container between the children and the grandparent
function offsetChildrenByDelta(
  childInstances: Array<ElementInstanceMetadata>,
  boundingFrame: CanvasRectangle,
) {
  return childInstances.flatMap((child) =>
    child.globalFrame != null && isFiniteRectangle(child.globalFrame)
      ? setElementTopLeft(child, {
          top: child.globalFrame.y - boundingFrame.y,
          left: child.globalFrame.x - boundingFrame.x,
        })
      : [],
  )
}

export function convertFrameToGroupCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentOffset =
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements.offset ??
    zeroCanvasPoint

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceContentAffectingPathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      MetadataUtils.getChildrenPathsUnordered(metadata, elementPath),
    ),
  )

  return [
    ...nukeAllAbsolutePositioningPropsCommands(elementPath),
    nukeSizingPropsForAxisCommand('vertical', elementPath),
    nukeSizingPropsForAxisCommand('horizontal', elementPath),
    ...offsetChildrenByVectorCommands(childInstances, parentOffset),
  ]
}

export function convertGroupToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CanvasCommand[] | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return null
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceContentAffectingPathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      MetadataUtils.getChildrenPathsUnordered(metadata, elementPath),
    ),
  )

  return [
    setProperty('always', elementPath, PP.create('style', 'position'), 'absolute'),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'left'),
      setExplicitCssValue(cssPixelLength(left)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'top'),
      setExplicitCssValue(cssPixelLength(top)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'width'),
      setExplicitCssValue(cssPixelLength(childrenBoundingFrame.width)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'height'),
      setExplicitCssValue(cssPixelLength(childrenBoundingFrame.height)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function groupConversionCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> | null {
  const contentAffectingType = getElementContentAffectingType(
    metadata,
    allElementProps,
    elementPath,
  )

  if (contentAffectingType === 'fragment' || contentAffectingType === 'conditional') {
    return null
  }

  if (contentAffectingType === 'sizeless-div') {
    const convertCommands = convertGroupToFrameCommands(metadata, allElementProps, elementPath)
    if (convertCommands != null) {
      return convertCommands
    }
  }

  const isProbablyPositionAbsoluteContainer = isAbsolutePositionedFrame(
    metadata,
    allElementProps,
    elementPath,
  )

  if (isProbablyPositionAbsoluteContainer) {
    return convertFrameToGroupCommands(metadata, allElementProps, elementPath)
  }

  return null
}
