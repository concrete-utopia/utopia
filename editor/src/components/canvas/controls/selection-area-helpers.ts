import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasRectangle,
  WindowPoint,
  canvasRectangle,
  isFiniteRectangle,
  rectangleContainsRectangle,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'

type ElementUnderSelectionAreaType = 'scene-child' | 'scene-or-scene-root' | 'storyboard-child'

type ElementUnderSelectionArea = {
  path: ElementPath
  type: ElementUnderSelectionAreaType
  fullyContained: boolean
}

function getElementUnderSelectionAreaType(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): ElementUnderSelectionAreaType | null {
  if (MetadataUtils.isProbablyScene(metadata, EP.nthParentPath(path, 3))) {
    return 'scene-child'
  }
  if (
    MetadataUtils.isProbablyScene(metadata, EP.nthParentPath(path, 1)) ||
    MetadataUtils.isProbablyScene(metadata, EP.nthParentPath(path, 2))
  ) {
    return null
  }
  if (MetadataUtils.isProbablyScene(metadata, path)) {
    return 'scene-or-scene-root'
  }
  return 'storyboard-child'
}

function elementIsFullyContainedInArea(
  area: CanvasRectangle,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  return element == null || element.globalFrame == null || !isFiniteRectangle(element.globalFrame)
    ? false
    : rectangleContainsRectangle(area, element.globalFrame)
}

export const filterUnderSelectionArea = (
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  area: CanvasRectangle,
): ElementPath[] => {
  const elements: ElementUnderSelectionArea[] = mapDropNulls((path) => {
    const type = getElementUnderSelectionAreaType(metadata, path)
    if (type == null) {
      return null
    }
    return {
      path,
      type: type,
      fullyContained: elementIsFullyContainedInArea(area, path, metadata),
    }
  }, paths)

  const thereAreStoryboardChildren = elements.some((other) => other.type === 'storyboard-child')

  return elements
    .filter((element) => {
      // only outermost children
      if (
        element.type === 'storyboard-child' &&
        elements.some((other) => {
          return other.type === 'storyboard-child' && EP.isDescendantOf(element.path, other.path)
        })
      ) {
        return false
      }
      // if the element is a schene child and there are storyboard children, skip it
      if (element.type === 'scene-child' && thereAreStoryboardChildren) {
        return false
      }
      // if the element is a scene, and the scene is not fully contained skip the scene
      if (element.type === 'scene-or-scene-root' && !element.fullyContained) {
        return false
      }
      // if a scene is fully contained, select just the scene and omit its children
      if (element.type === 'scene-child') {
        const parentScene = elements.find(
          (other) =>
            other.type === 'scene-or-scene-root' && EP.isDescendantOf(element.path, other.path),
        )
        if (parentScene != null && parentScene.fullyContained) {
          return false
        }
      }
      return true
    })
    .map((r) => r.path)
}

export function getSelectionAreaRenderedRect(
  selectionArea: CanvasRectangle | null,
  boundingRect: DOMRect | null,
): CanvasRectangle | null {
  if (selectionArea == null || boundingRect == null) {
    return null
  }
  return canvasRectangle({
    x: selectionArea.x - boundingRect.x,
    y: selectionArea.y - boundingRect.y,
    width: selectionArea.width,
    height: selectionArea.height,
  })
}

export function makeSelectionArea(from: WindowPoint, to: WindowPoint): CanvasRectangle {
  return canvasRectangle({
    x: Math.min(from.x, to.x),
    y: Math.min(from.y, to.y),
    width: Math.max(from.x, to.x) - Math.min(from.x, to.x),
    height: Math.max(from.y, to.y) - Math.min(from.y, to.y),
  })
}

export function isValidMouseEventForSelectionArea(e: React.MouseEvent): boolean {
  return e.button === 0 && !(e.shiftKey || e.metaKey || e.ctrlKey || e.altKey)
}
