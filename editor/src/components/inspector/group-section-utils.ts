import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { nullIfInfinity } from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { treatElementAsContentAffecting } from '../canvas/canvas-strategies/strategies/group-like-helpers'
import { AllElementProps } from '../editor/store/editor-state'
import * as EP from '../../core/shared/element-path'

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
  elementPath: ElementPath,
): { top: number; left: number } | null {
  const children = MetadataUtils.getChildrenUnordered(metadata, elementPath)
  const childOffsets = children.map((child) => child.specialSizeMeasurements.offset)
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
  if (
    EP.isStoryboardChild(elementPath) ||
    treatElementAsContentAffecting(metadata, allElementProps, elementPath)
  ) {
    return isStoryboardOrGroupChild(metadata, allElementProps, EP.parentPath(elementPath))
  }
  return EP.isStoryboardChild(elementPath)
}
