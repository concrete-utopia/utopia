import type { HonoursPosition } from '../../../../core/model/element-template-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { uniqBy } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import { treatElementAsGroupLike } from './group-helpers'

export function supportsStyle(canvasState: InteractionCanvasState, element: ElementPath): boolean {
  return MetadataUtils.targetUsesProperty(
    canvasState.projectContents,
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
    'style',
  )
}

export function honoursPropsSize(
  canvasState: InteractionCanvasState,
  element: ElementPath,
): boolean {
  return MetadataUtils.targetHonoursPropsSize(
    canvasState.projectContents,
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
  )
}

export function honoursPropsPosition(
  canvasState: InteractionCanvasState,
  element: ElementPath,
): HonoursPosition {
  return MetadataUtils.targetHonoursPropsPosition(
    canvasState.projectContents,
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
  )
}

export function shouldKeepMovingDraggedGroupChildren(
  jsxMetadata: ElementInstanceMetadataMap,
  paths: ElementPath[],
  reparentTarget: InsertionPath,
): boolean {
  // all selected elements must be under the same non-root hierarchy
  const commonParent = EP.getCommonParent(paths, true)
  if (commonParent == null || EP.isStoryboardPath(commonParent)) {
    return false
  }

  // all elements must be group children
  if (!paths.every((path) => treatElementAsGroupLike(jsxMetadata, EP.parentPath(path)))) {
    return false
  }

  // all elements must not be crossing scenes
  const sceneAncestors = uniqBy(
    paths.flatMap((path) => EP.getAncestors(path)),
    EP.pathsEqual,
  ).filter((path) => MetadataUtils.isProbablyScene(jsxMetadata, path))
  if (sceneAncestors.length > 0) {
    const commonSceneParent = EP.getCommonParent(
      [...sceneAncestors, reparentTarget.intendedParentPath],
      true,
    )
    if (
      commonSceneParent == null ||
      !MetadataUtils.isProbablyScene(jsxMetadata, commonSceneParent)
    ) {
      return false
    }
  }

  return true
}
