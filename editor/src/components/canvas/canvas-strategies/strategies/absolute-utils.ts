import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { InteractionCanvasState } from '../canvas-strategy-types'

export function supportsStyle(canvasState: InteractionCanvasState, element: ElementPath): boolean {
  return MetadataUtils.targetUsesProperty(
    canvasState.projectContents,
    canvasState.openFile,
    element,
    'style',
  )
}

export function honoursPropsSize(
  canvasState: InteractionCanvasState,
  element: ElementPath,
): boolean {
  return MetadataUtils.targetHonoursPropsSize(
    canvasState.projectContents,
    canvasState.openFile,
    element,
  )
}

export function honoursPropsPosition(
  canvasState: InteractionCanvasState,
  element: ElementPath,
): boolean {
  return MetadataUtils.targetHonoursPropsPosition(
    canvasState.projectContents,
    canvasState.openFile,
    element,
  )
}
