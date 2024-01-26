import type { ThreadMetadata } from '../../../liveblocks.config'
import type { CanvasPoint, LocalPoint } from '../shared/math-utils'
import { canvasPoint, localPoint } from '../shared/math-utils'
import { assertNever } from '../shared/utils'

export type UtopiaThreadMetadata = CanvasThreadMetadata | SceneThreadMetadata

type BaseThreadMetadata = {
  position: CanvasPoint
  resolved: boolean
}

export type CanvasThreadMetadata = BaseThreadMetadata & {
  type: 'canvas'
}

export type SceneThreadMetadata = BaseThreadMetadata & {
  type: 'scene'
  sceneId: string
  scenePosition: LocalPoint
  remixLocationRoute?: string
}

export function canvasThreadMetadata(
  data: Omit<CanvasThreadMetadata, 'type'>,
): CanvasThreadMetadata {
  return { ...data, type: 'canvas' }
}

export function sceneThreadMetadata(data: Omit<SceneThreadMetadata, 'type'>): SceneThreadMetadata {
  return { ...data, type: 'scene' }
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

export function liveblocksThreadMetadataToUtopia(metadata: ThreadMetadata): UtopiaThreadMetadata {
  if (metadata.sceneId != null && metadata.sceneX != null && metadata.sceneY != null) {
    return sceneThreadMetadata({
      position: canvasPoint(metadata),
      sceneId: metadata.sceneId,
      scenePosition: localPoint({ x: metadata.sceneX, y: metadata.sceneY }),
      resolved: metadata.resolved,
    })
  } else {
    return canvasThreadMetadata({
      position: canvasPoint(metadata),
      resolved: metadata.resolved,
    })
  }
}

export function utopiaThreadMetadataToLiveblocks(metadata: UtopiaThreadMetadata): ThreadMetadata {
  switch (metadata.type) {
    case 'canvas':
      return {
        x: metadata.position.x,
        y: metadata.position.y,
        resolved: metadata.resolved,
      }
    case 'scene':
      return {
        x: metadata.position.x,
        y: metadata.position.y,
        sceneId: metadata.sceneId,
        sceneX: metadata.scenePosition.x,
        sceneY: metadata.scenePosition.y,
        resolved: metadata.resolved,
        remixLocationRoute: metadata.remixLocationRoute,
      }
    default:
      assertNever(metadata)
  }
}

type PartialNullable<T> = {
  [P in keyof T]?: T[P] | null | undefined
}

type UtopiaThreadMetadataQuery = PartialNullable<UtopiaThreadMetadata> &
  Pick<UtopiaThreadMetadata, 'type'>

export function utopiaThreadMetadataToLiveblocksPartial(
  metadata: UtopiaThreadMetadataQuery,
): PartialNullable<ThreadMetadata> {
  switch (metadata.type) {
    case 'scene':
      return {
        x: metadata.position?.x,
        y: metadata.position?.y,
        sceneId: metadata.sceneId,
        sceneX: metadata.scenePosition?.x,
        sceneY: metadata.scenePosition?.y,
        resolved: metadata.resolved,
        remixLocationRoute: metadata.remixLocationRoute,
      }
    case 'canvas':
      return {
        x: metadata.position?.x,
        y: metadata.position?.y,
        sceneId: null, // the null fields are necessary so we delete these fields on update from liveblocks
        sceneX: null,
        sceneY: null,
        remixLocationRoute: null,
        resolved: metadata.resolved,
      }
    default:
      assertNever(metadata)
  }
}
