import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasRectangle,
  WindowPoint,
  WindowRectangle,
  isFiniteRectangle,
  rectangleContainsRectangle,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { KeysPressed } from '../../../utils/keyboard'
import { InteractionSession, isDragToPan } from '../canvas-strategies/interaction-state'

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
      // no zero-sized elements
      if (element.zeroSized) {
        return false
      }

      // only outermost children
      if (
        element.type === 'regular' &&
        elements.some((other) => {
          return (
            other.type !== 'scene' &&
            !other.zeroSized &&
            EP.isDescendantOf(element.path, other.path)
          )
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
      return true
    })
    .map((r) => r.path)
}

export function getSelectionAreaRenderedRect(
  selectionArea: WindowRectangle | null,
  boundingRect: DOMRect | null,
): WindowRectangle | null {
  if (selectionArea == null || boundingRect == null) {
    return null
  }
  return {
    x: selectionArea.x - boundingRect.x,
    y: selectionArea.y - boundingRect.y,
    width: selectionArea.width,
    height: selectionArea.height,
  } as WindowRectangle
}

export function makeSelectionArea(from: WindowPoint, to: WindowPoint): WindowRectangle {
  return {
    x: Math.min(from.x, to.x),
    y: Math.min(from.y, to.y),
    width: Math.max(from.x, to.x) - Math.min(from.x, to.x),
    height: Math.max(from.y, to.y) - Math.min(from.y, to.y),
  } as WindowRectangle
}

export function isValidMouseEventForSelectionArea(
  e: MouseEvent | React.MouseEvent,
  interactionSession: InteractionSession | null,
  keysPressed: KeysPressed,
): boolean {
  return (
    e.button === 0 &&
    !(e.shiftKey || e.metaKey || e.ctrlKey || e.altKey) &&
    !isDragToPan(interactionSession, keysPressed['space'])
  )
}
