import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasRectangle,
  WindowPoint,
  canvasRectangle,
  isFiniteRectangle,
  rectContainsPoint,
  rectangleContainsRectangle,
  rectanglesOverlap,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'

export function getPossibleElementsUnderMouse(
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
): ElementInstanceMetadata[] {
  const selectableElements = mapDropNulls((path) => {
    return MetadataUtils.findElementByElementPath(metadata, path)
  }, MetadataUtils.getAllCanvasSelectablePathsUnordered(metadata))

  const nonSelectableElementsPossiblyUnderMouse = Object.values(metadata).filter((e) =>
    selectableElements.some((other) => EP.isDescendantOf(e.elementPath, other.elementPath)),
  )

  return [
    ...selectableElements,
    ...nonSelectableElementsPossiblyUnderMouse,
    ...mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      selectedViews,
    ),
  ]
}

export const getElementsUnderSelectionArea = (
  metadata: ElementInstanceMetadataMap,
  selectionAreaCanvasRect: CanvasRectangle,
): ElementPath[] => {
  const possibleElementsUnderSelectionArea = mapDropNulls((element) => {
    if (element.globalFrame == null || !isFiniteRectangle(element.globalFrame)) {
      return null
    }
    const isChildOfSceneRoot = MetadataUtils.isProbablyScene(
      metadata,
      EP.nthParentPath(element.elementPath, 3),
    )
    if (!(isChildOfSceneRoot || EP.isStoryboardPath(EP.parentPath(element.elementPath)))) {
      return null
    }

    return {
      path: element.elementPath,
      frame: element.globalFrame,
      type: isChildOfSceneRoot
        ? 'scene-child'
        : MetadataUtils.isProbablyScene(metadata, element.elementPath)
        ? 'scene'
        : 'storyboard-child',
    }
  }, Object.values(metadata))

  const allElementsUnderSelectionArea = mapDropNulls((element) => {
    if (!rectanglesOverlap(element.frame, selectionAreaCanvasRect)) {
      return null
    }
    return {
      ...element,
      fullyCovered: rectangleContainsRectangle(selectionAreaCanvasRect, element.frame),
    }
  }, possibleElementsUnderSelectionArea)

  const thereAreStoryboardChildren = allElementsUnderSelectionArea.some(
    (other) => other.type === 'storyboard-child',
  )

  return allElementsUnderSelectionArea
    .filter((element) => {
      // if the element is a schene child and there are storyboard children, skip it
      if (element.type === 'scene-child' && thereAreStoryboardChildren) {
        return false
      }
      // if the element is a scene, and the scene is not fully covered skip the scene
      if (element.type === 'scene' && !element.fullyCovered) {
        return false
      }
      // if a scene is fully covered, select just the scene and omit its children
      if (element.type === 'scene-child') {
        const parentScene = allElementsUnderSelectionArea.find(
          (other) => other.type === 'scene' && EP.isDescendantOf(element.path, other.path),
        )
        if (parentScene != null && parentScene.fullyCovered) {
          return false
        }
      }
      return true
    })
    .map((r) => r.path)
}

export const elementIsUnderMouse =
  (mousePointOnCanvas: CanvasPoint | null) =>
  (element: ElementInstanceMetadata | null): boolean => {
    return mousePointOnCanvas == null || element == null
      ? false
      : element.globalFrame != null &&
          isFiniteRectangle(element.globalFrame) &&
          rectContainsPoint(element.globalFrame, mousePointOnCanvas)
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
