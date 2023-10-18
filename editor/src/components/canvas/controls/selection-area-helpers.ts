import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type {
  CanvasRectangle,
  CanvasVector,
  WindowRectangle,
} from '../../../core/shared/math-utils'
import {
  canvasPoint,
  isFiniteRectangle,
  rectangleContainsRectangle,
  rectangleIntersection,
  windowRectangle,
  zeroRectangle,
} from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { KeysPressed } from '../../../utils/keyboard'
import type { InteractionSession } from '../canvas-strategies/interaction-state'
import { isDragToPan } from '../canvas-strategies/interaction-state'
import { canvasPointToWindowPoint, getAllTargetsAtPoint } from '../dom-lookup'

type ElementUnderSelectionAreaType = 'scene' | 'regular'

type ElementUnderSelectionArea = {
  path: ElementPath
  type: ElementUnderSelectionAreaType
  fullyContained: boolean
  selected: boolean
  zeroSized: boolean
}

function getElementUnderSelectionAreaType(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): ElementUnderSelectionAreaType {
  return MetadataUtils.isProbablyScene(metadata, path) ? 'scene' : 'regular'
}

function maybeElementFrame(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): CanvasRectangle | null {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (element == null || element.globalFrame == null) {
    return null
  }
  if (!isFiniteRectangle(element.globalFrame)) {
    return null
  }
  return element.globalFrame
}

export const filterUnderSelectionArea = (
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  scale: number,
  canvasOffset: CanvasVector,
  area: CanvasRectangle | null,
  selectedViews: ElementPath[],
): ElementPath[] => {
  if (area == null) {
    return []
  }

  const elements: ElementUnderSelectionArea[] = paths.map((path) => {
    const frame = maybeElementFrame(path, metadata)
    return {
      path: path,
      type: getElementUnderSelectionAreaType(metadata, path),
      fullyContained: frame != null && rectangleContainsRectangle(area, frame),
      selected: EP.containsPath(path, selectedViews),
      zeroSized: frame == null || frame.width === 0 || frame.height === 0,
    }
  })

  const isSceneChild =
    (element: ElementUnderSelectionArea) => (other: ElementUnderSelectionArea) => {
      return other.type === 'scene' && EP.isDescendantOf(element.path, other.path)
    }

  const thereAreRegularElements = elements.some(
    (element) => element.type === 'regular' && !elements.some(isSceneChild(element)),
  )

  return elements
    .filter((element) => {
      // only outermost children
      if (
        element.type === 'regular' &&
        elements.some((other) => {
          return other.type !== 'scene' && EP.isDescendantOf(element.path, other.path)
        })
      ) {
        return false
      }

      const parentScene = elements.find(isSceneChild(element))

      // if the element is a scene child and there are storyboard children, skip it
      if (parentScene != null && thereAreRegularElements) {
        return false
      }
      // if the element is a scene, and the scene is not fully contained skip the scene
      if (element.type === 'scene' && !element.fullyContained && !element.selected) {
        return false
      }
      // if a scene is fully contained, select just the scene and omit its children
      if (parentScene != null && parentScene.fullyContained) {
        return false
      }

      // NOTE: this is a mitigation step for a measuring problem, where overflowing elements'
      // dimensions are not calculated correctly. This should be fixed at the root in the measurements,
      // but until then this should help a bit.
      if (
        element.type === 'regular' &&
        !element.zeroSized &&
        !element.selected &&
        parentScene == null
      ) {
        return isElementIntersactionActuallyUnderAreaAndVisible(
          metadata,
          scale,
          canvasOffset,
          area,
          element.path,
        )
      }
      return true
    })
    .map((r) => r.path)
}

export function getSelectionAreaRenderedRect(
  selectionArea: WindowRectangle | null,
  boundingRect: DOMRect | null,
  scale: number,
): WindowRectangle | null {
  if (selectionArea == null || boundingRect == null) {
    return null
  }
  const scaleFactor = scale > 1 ? scale : 1
  return windowRectangle({
    x: selectionArea.x - boundingRect.x * scaleFactor,
    y: selectionArea.y - boundingRect.y * scaleFactor,
    width: selectionArea.width,
    height: selectionArea.height,
  })
}

export function isValidMouseEventForSelectionArea(
  e: MouseEvent | React.MouseEvent,
  interactionSession: InteractionSession | null,
  keysPressed: KeysPressed,
): boolean {
  return (
    e.button === 0 &&
    !(e.metaKey || e.ctrlKey || e.altKey) &&
    !isDragToPan(interactionSession, keysPressed['space'])
  )
}

function isElementIntersactionActuallyUnderAreaAndVisible(
  jsxMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
  canvasOffset: CanvasVector,
  area: CanvasRectangle | null,
  path: ElementPath,
): boolean {
  const frame = MetadataUtils.getFrameInCanvasCoords(path, jsxMetadata)
  if (area != null && frame != null && isFiniteRectangle(frame)) {
    const intersect = rectangleIntersection(area, frame) ?? zeroRectangle
    if (intersect == null) {
      return false
    }
    const pathActuallyUnderArea = getAllTargetsAtPoint(
      [path],
      canvasPointToWindowPoint(
        canvasPoint({ x: intersect.x + 1, y: intersect.y + 1 }),
        canvasScale,
        canvasOffset,
      ),
      canvasScale,
      canvasOffset,
      jsxMetadata,
    ).some((other) => EP.pathsEqual(path, other))
    if (!pathActuallyUnderArea) {
      return false
    }
  }
  return true
}
