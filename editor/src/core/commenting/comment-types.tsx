import type { ThreadMetadata } from '../../../liveblocks.config'
import type { CanvasPoint, LocalPoint } from '../shared/math-utils'
import { canvasPoint, localPoint } from '../shared/math-utils'

export function liveblocksThreadMetadataToUtopia(metadata: ThreadMetadata): UtopiaThreadMetadata {
  if (metadata.sceneId != null && metadata.sceneX != null && metadata.sceneY != null) {
    return {
      type: 'scene',
      position: canvasPoint(metadata),
      sceneId: metadata.sceneId,
      scenePosition: localPoint({ x: metadata.sceneX, y: metadata.sceneY }),
      resolved: metadata.resolved,
    }
  } else {
    return {
      type: 'canvas',
      position: canvasPoint(metadata),
      resolved: metadata.resolved,
    }
  }
}

export function utopiaThreadMetadataToLiveblocks(metadata: UtopiaThreadMetadata): ThreadMetadata {
  switch (metadata.type) {
    case 'canvas':
      return {
        x: metadata.position.x,
        y: metadata.position.y,
        sceneId: undefined, // the undefined fields are necessary so we delete these fields on update from liveblocks
        sceneX: undefined,
        sceneY: undefined,
        resolved: metadata.resolved,
        remixLocationRoute: metadata.remixLocationRoute,
      }
    case 'scene':
      return {
        x: metadata.position.x,
        y: metadata.position.y,
        sceneId: metadata.sceneId,
        sceneX: metadata.scenePosition.x,
        sceneY: metadata.scenePosition.x,
        resolved: metadata.resolved,
        remixLocationRoute: metadata.remixLocationRoute,
      }
  }
}

export type UtopiaThreadMetadata = CanvasThreadMetadata | SceneThreadMetadata

type BaseThreadMetadata = {
  position: CanvasPoint
  remixLocationRoute?: string
  resolved: boolean
}

export type CanvasThreadMetadata = BaseThreadMetadata & {
  type: 'canvas'
}

export function canvasMetadata(
  position: CanvasPoint,
  remixLocationRoute?: string,
  resolved: boolean = false,
): CanvasThreadMetadata {
  return {
    type: 'canvas',
    position: position,
    remixLocationRoute: remixLocationRoute,
    resolved: resolved,
  }
}

export type SceneThreadMetadata = BaseThreadMetadata & {
  type: 'scene'
  sceneId: string
  scenePosition: LocalPoint
}

export function sceneThreadMetadata(
  position: CanvasPoint,
  sceneId: string,
  scenePosition: LocalPoint,
  remixLocationRoute?: string,
  resolved: boolean = false,
): SceneThreadMetadata {
  return {
    type: 'scene',
    position: position,
    sceneId: sceneId,
    scenePosition: scenePosition,
    remixLocationRoute: remixLocationRoute,
    resolved: resolved,
  }
}

export function isSceneThreadMetadata(
  metadata: UtopiaThreadMetadata,
): metadata is SceneThreadMetadata {
  return metadata.type === 'scene'
}

export function isCanvasThreadMetadata(
  metadata: UtopiaThreadMetadata,
): metadata is CanvasThreadMetadata {
  return metadata.type === 'canvas'
}
