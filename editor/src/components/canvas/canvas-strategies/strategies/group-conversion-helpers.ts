import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  zeroCanvasPoint,
  isFiniteRectangle,
  isInfinityRectangle,
  zeroCanvasRect,
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

function isAbsolutePositionedFrame(
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

function convertFrameToGroupCommands(
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
    ...childInstances.flatMap((child) =>
      child.globalFrame != null && isFiniteRectangle(child.globalFrame)
        ? setElementTopLeft(child, {
            top: child.specialSizeMeasurements.offset.y + parentOffset.y,
            left: child.specialSizeMeasurements.offset.x + parentOffset.x,
          })
        : [],
    ),
  ]
}

function convertGroupToFrameCommands(
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
    ...childInstances.flatMap((child) =>
      child.globalFrame != null && isFiniteRectangle(child.globalFrame)
        ? setElementTopLeft(child, {
            top: child.globalFrame.y - childrenBoundingFrame.y,
            left: child.globalFrame.x - childrenBoundingFrame.x,
          })
        : [],
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
  ]
}

export function convertToGroupCommands(
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
