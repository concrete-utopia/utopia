import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import {
  CanvasPoint,
  isInfinityRectangle,
  LocalRectangle,
  nullIfInfinity,
} from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import {
  replaceContentAffectingPathsWithTheirChildrenRecursive,
  treatElementAsContentAffecting,
} from '../canvas/canvas-strategies/strategies/group-like-helpers'
import { AllElementProps } from '../editor/store/editor-state'
import * as EP from '../../core/shared/element-path'
import { CanvasCommand } from '../canvas/commands/commands'
import { setCssLengthProperty } from '../canvas/commands/set-css-length-command'
import { styleP } from './inspector-common'
import { cssNumber } from './common/css-utils'
import { convertToAbsolute } from '../canvas/commands/convert-to-absolute-command'

export function getChildrenOffsetFromGlobalFrame(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): { top: number; left: number } | null {
  const children = MetadataUtils.getChildrenUnordered(metadata, elementPath)
  const childFrames = mapDropNulls((child) => nullIfInfinity(child.globalFrame), children)
  if (childFrames.length === 0) {
    return null
  }
  const top = childFrames.reduce((acc, val) => Math.min(acc, val.y), childFrames[0].y)
  const left = childFrames.reduce((acc, val) => Math.min(acc, val.x), childFrames[0].x)
  return { top, left }
}

export function getChildrenOffsets(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): { top: number; left: number } | null {
  const children = replaceContentAffectingPathsWithTheirChildrenRecursive(
    metadata,
    allElementProps,
    MetadataUtils.getChildrenPathsUnordered(metadata, elementPath),
  )
  const childOffsets = mapDropNulls(
    (child) =>
      MetadataUtils.findElementByElementPath(metadata, child)?.specialSizeMeasurements?.offset ??
      null,
    children,
  )

  if (childOffsets.length === 0) {
    return null
  }
  const top = childOffsets.reduce((acc, val) => Math.min(acc, val.y), childOffsets[0].y)
  const left = childOffsets.reduce((acc, val) => Math.min(acc, val.x), childOffsets[0].x)
  return { top, left }
}

export function isStoryboardOrGroupChild(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): boolean {
  if (EP.isStoryboardPath(elementPath) || EP.isStoryboardChild(elementPath)) {
    return true
  }

  if (treatElementAsContentAffecting(metadata, allElementProps, elementPath)) {
    return isStoryboardOrGroupChild(metadata, allElementProps, EP.parentPath(elementPath))
  }

  return false
}

export function isDescendantOfGroups(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): boolean {
  if (EP.isStoryboardPath(elementPath) || EP.isStoryboardChild(elementPath)) {
    return true
  }

  if (treatElementAsContentAffecting(metadata, allElementProps, elementPath)) {
    return isDescendantOfGroups(metadata, allElementProps, EP.parentPath(elementPath))
  }

  return false
}

export interface PositioningProps {
  width: number
  height: number
  top: number
  left: number
}

export function getWrapperPositioningProps(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): PositioningProps | null {
  const frame = MetadataUtils.getChildrenGlobalBoundingBox(metadata, elementPath)
  const offset = getChildrenOffsetFromGlobalFrame(metadata, elementPath)

  if (frame == null || offset == null) {
    return null
  }

  return { top: offset.top, left: offset.left, width: frame.width, height: frame.height }
}

export function getChildFrameAdjustCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  parentOffset: CanvasPoint,
): CanvasCommand[] {
  const childrenPaths = replaceContentAffectingPathsWithTheirChildrenRecursive(
    metadata,
    allElementProps,
    MetadataUtils.getChildrenPathsUnordered(metadata, elementPath),
  )

  const childLocalFrames = mapDropNulls((childPath): [LocalRectangle, ElementPath] | null => {
    const childInstance = MetadataUtils.findElementByElementPath(metadata, childPath)
    if (childInstance == null) {
      return null
    }

    const frame = childInstance.localFrame
    if (frame == null || isInfinityRectangle(frame)) {
      return null
    }

    return [frame, childPath]
  }, childrenPaths)

  const adjustTopCommands: CanvasCommand[] = childLocalFrames.map(([frame, targetPath]) =>
    setCssLengthProperty(
      'always',
      targetPath,
      styleP('top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(frame.y - parentOffset.y, null) },
      null,
    ),
  )

  const adjustLeftCommands: CanvasCommand[] = childLocalFrames.map(([frame, targetPath]) =>
    setCssLengthProperty(
      'always',
      targetPath,
      styleP('left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(frame.x - parentOffset.x, null) },
      null,
    ),
  )

  const childSetPositionAbsoluteCommands = childLocalFrames.map(([_, targetPath]) =>
    convertToAbsolute('always', targetPath),
  )

  return [...adjustTopCommands, ...adjustLeftCommands, ...childSetPositionAbsoluteCommands]
}
