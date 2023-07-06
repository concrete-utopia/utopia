import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { InteractionCanvasState } from '../canvas-strategy-types'

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
): boolean {
  return MetadataUtils.targetHonoursPropsPosition(
    canvasState.projectContents,
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
  )
}
