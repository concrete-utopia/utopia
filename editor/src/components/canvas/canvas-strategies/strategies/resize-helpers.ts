import { styleStringInArray } from '../../../../utils/common-constants'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { defaultEither, isRight, mapEither } from '../../../../core/shared/either'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import { getJSXAttribute } from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  distance,
  defaultIfNaN,
  offsetPoint,
  pointDifference,
  rectFromTwoPoints,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import { AspectRatioLockedProp } from '../../../aspect-ratio'
import type { EdgePosition } from '../../canvas-types'
import { CSSCursor } from '../../canvas-types'
import {
  isEdgePositionAHorizontalEdge,
  isEdgePositionAVerticalEdge,
  isEdgePositionOnSide,
  pickPointOnRect,
} from '../../canvas-utils'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { honoursPropsToPositionElement, honoursPropsToSizeElement } from './absolute-utils'

export type AbsolutePin = 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'

export const allPins: Array<AbsolutePin> = ['top', 'left', 'width', 'height', 'bottom', 'right']
export const horizontalPins: Array<AbsolutePin> = ['left', 'width', 'right']
export const verticalPins: Array<AbsolutePin> = ['top', 'height', 'bottom']

export function isHorizontalPin(pin: AbsolutePin): boolean {
  return horizontalPins.includes(pin)
}

export function hasAtLeastTwoPinsPerSide(props: { [key: string]: any }): boolean {
  return (
    horizontalPins.filter((pin) => props.style?.[pin] != null).length >= 2 &&
    verticalPins.filter((pin) => props.style?.[pin] != null).length >= 2
  )
}

export function onlyEnsureOffsetPinsExist(
  props: PropsOrJSXAttributes,
  edgePosition: EdgePosition,
): Array<AbsolutePin> {
  const existingHorizontalPins = horizontalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, styleStringInArray)
    return isRight(prop) && prop.value != null
  })
  const existingVerticalPins = verticalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, styleStringInArray)
    return isRight(prop) && prop.value != null
  })
  if (existingHorizontalPins.length === 0) {
    existingHorizontalPins.push('left')
  }
  if (existingVerticalPins.length === 0) {
    existingVerticalPins.push('top')
  }
  return [...existingHorizontalPins, ...existingVerticalPins]
}

export function ensureAtLeastTwoPinsForEdgePosition(
  props: PropsOrJSXAttributes,
  edgePosition: EdgePosition,
): Array<AbsolutePin> {
  const existingHorizontalPins = horizontalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, styleStringInArray)
    return isRight(prop) && prop.value != null
  })
  const existingVerticalPins = verticalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, styleStringInArray)
    return isRight(prop) && prop.value != null
  })

  const horizontalPinsToAdd: Array<AbsolutePin> = [...existingHorizontalPins]
  if (edgePosition.x !== 0.5) {
    if (existingHorizontalPins.length === 0) {
      horizontalPinsToAdd.push('left')
      horizontalPinsToAdd.push('width')
    } else if (existingHorizontalPins.length === 1) {
      if (existingHorizontalPins.includes('width')) {
        horizontalPinsToAdd.push('left')
      } else {
        horizontalPinsToAdd.push('width')
      }
    }
  }
  const verticalPinsToAdd: Array<AbsolutePin> = [...existingVerticalPins]
  if (edgePosition.y !== 0.5) {
    if (existingVerticalPins.length === 0) {
      verticalPinsToAdd.push('top')
      verticalPinsToAdd.push('height')
    } else if (existingVerticalPins.length === 1) {
      if (existingVerticalPins.includes('height')) {
        verticalPinsToAdd.push('top')
      } else {
        verticalPinsToAdd.push('height')
      }
    }
  }

  return [...horizontalPinsToAdd, ...verticalPinsToAdd]
}

export function supportsAbsoluteResize(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
  canvasState: InteractionCanvasState,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
  return (
    elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
    honoursPropsToPositionElement(canvasState, element) &&
    honoursPropsToSizeElement(canvasState, element)
  )
}

export function getLockedAspectRatio(
  interactionData: InteractionSession,
  modifiers: Modifiers,
  originalBoundingBox: CanvasRectangle,
  anySelectedElementAspectRatioLocked: boolean,
): number | null {
  if (interactionData.aspectRatioLock != null) {
    return interactionData.aspectRatioLock
  }

  if (anySelectedElementAspectRatioLocked && modifiers.shift) {
    return null
  }

  if (anySelectedElementAspectRatioLocked || modifiers.shift) {
    const rawAspectRatio = originalBoundingBox.width / originalBoundingBox.height
    return defaultIfNaN(rawAspectRatio, 1)
  }

  return null
}

function isElementImage(instance: ElementInstanceMetadata) {
  return defaultEither(
    false,
    mapEither((e) => e.type === 'JSX_ELEMENT' && e.name.baseVariable === 'img', instance.element),
  )
}

function isElementAspectRatioLocked(instance: ElementInstanceMetadata) {
  return defaultEither(
    false,
    mapEither(
      (e) => e.type === 'JSX_ELEMENT' && getJSXAttribute(e.props, AspectRatioLockedProp),
      instance.element,
    ),
  )
}

export function isAnySelectedElementAspectRatioLocked(
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): boolean {
  return MetadataUtils.findElementsByElementPath(jsxMetadata, selectedElements).some(
    (e) => isElementImage(e) || isElementAspectRatioLocked(e),
  )
}

export function pickCursorFromEdgePosition(edgePosition: EdgePosition) {
  const isTopLeft = edgePosition.x === 0 && edgePosition.y === 0
  const isBottomRight = edgePosition.x === 1 && edgePosition.y === 1

  if (isEdgePositionAHorizontalEdge(edgePosition)) {
    return CSSCursor.ResizeNS
  } else if (isEdgePositionAVerticalEdge(edgePosition)) {
    return CSSCursor.ResizeEW
  } else if (isTopLeft || isBottomRight) {
    return CSSCursor.ResizeNWSE
  } else {
    return CSSCursor.ResizeNESW
  }
}

export type IsCenterBased = 'center-based' | 'non-center-based'

export function resizeBoundingBox(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  lockedAspectRatio: number | null,
  centerBased: IsCenterBased,
): CanvasRectangle {
  if (isEdgePositionOnSide(edgePosition)) {
    return resizeBoundingBoxFromSide(
      boundingBox,
      drag,
      edgePosition,
      centerBased,
      lockedAspectRatio,
    )
  } else {
    return resizeBoundingBoxFromCorner(
      boundingBox,
      drag,
      edgePosition,
      centerBased,
      lockedAspectRatio,
    )
  }
}

export function resizeBoundingBoxFromCorner(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  centerBased: IsCenterBased,
  lockedAspectRatio: number | null,
): CanvasRectangle {
  const startingCornerPosition = {
    x: 1 - edgePosition.x,
    y: 1 - edgePosition.y,
  } as EdgePosition

  let oppositeCorner = pickPointOnRect(boundingBox, startingCornerPosition)
  const draggedCorner = pickPointOnRect(boundingBox, edgePosition)
  const newCorner = offsetPoint(draggedCorner, drag)

  let newBoundingBox = boundingBox
  if (centerBased === 'center-based') {
    oppositeCorner = offsetPoint(oppositeCorner, pointDifference(drag, zeroCanvasPoint))
  }

  newBoundingBox = rectFromTwoPoints(oppositeCorner, newCorner)
  if (lockedAspectRatio != null) {
    // When aspect ratio is locked we extend the new bounding box in this way:
    // 1. the extended bounding box should fully contain the bounding box
    // 2. the extended rectangle should have the correct locked aspect ratio
    // 3. there is always a fixed point of the bounding box which should not move:
    //    - when it is a center based resize that fixed point is the center of the rectangle
    //    - otherwise it is the opposite point than what we drag
    const fixedEdgePosition = getFixedEdgePositionForAspectRatioLockResize(
      centerBased,
      newCorner,
      oppositeCorner,
    )

    return extendRectangleToAspectRatio(newBoundingBox, fixedEdgePosition, lockedAspectRatio)
  } else {
    return newBoundingBox
  }
}

export function resizeBoundingBoxFromSide(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  centerBased: IsCenterBased,
  lockedAspectRatio: number | null,
): CanvasRectangle {
  const isEdgeHorizontalSide = isEdgePositionAHorizontalEdge(edgePosition)

  const dragToUse = isEdgeHorizontalSide
    ? canvasPoint({
        x: 0,
        y: drag.y,
      })
    : canvasPoint({
        x: drag.x,
        y: 0,
      })

  const oppositeSideCenterPosition = isEdgeHorizontalSide
    ? ({
        x: edgePosition.x,
        y: 1 - edgePosition.y,
      } as EdgePosition)
    : ({
        x: 1 - edgePosition.x,
        y: edgePosition.y,
      } as EdgePosition)

  const draggedSideCenter = pickPointOnRect(boundingBox, edgePosition)
  const newSideCenter = offsetPoint(draggedSideCenter, dragToUse)

  let oppositeSideCenter = pickPointOnRect(boundingBox, oppositeSideCenterPosition)
  if (centerBased === 'center-based') {
    oppositeSideCenter = offsetPoint(
      oppositeSideCenter,
      pointDifference(dragToUse, zeroCanvasPoint),
    )
  }

  if (lockedAspectRatio == null) {
    const newCorner1 = isEdgeHorizontalSide
      ? canvasPoint({
          x: oppositeSideCenter.x - boundingBox.width / 2,
          y: oppositeSideCenter.y,
        })
      : canvasPoint({
          x: oppositeSideCenter.x,
          y: oppositeSideCenter.y - boundingBox.height / 2,
        })
    const newCorner2 = isEdgeHorizontalSide
      ? canvasPoint({
          x: newSideCenter.x + boundingBox.width / 2,
          y: newSideCenter.y,
        })
      : canvasPoint({
          x: newSideCenter.x,
          y: newSideCenter.y + boundingBox.height / 2,
        })
    return rectFromTwoPoints(newCorner1, newCorner2)
  } else {
    const dragDistance = distance(newSideCenter, oppositeSideCenter)
    if (isEdgeHorizontalSide) {
      const newWidth = dragDistance * lockedAspectRatio
      const newCorner1 = canvasPoint({
        x: oppositeSideCenter.x - newWidth / 2,
        y: oppositeSideCenter.y,
      })
      const newCorner2 = canvasPoint({
        x: newSideCenter.x + newWidth / 2,
        y: newSideCenter.y,
      })
      return rectFromTwoPoints(newCorner1, newCorner2)
    } else {
      const newHeight = dragDistance / lockedAspectRatio
      const newCorner1 = canvasPoint({
        x: oppositeSideCenter.x,
        y: oppositeSideCenter.y - newHeight / 2,
      })
      const newCorner2 = canvasPoint({
        x: newSideCenter.x,
        y: newSideCenter.y + newHeight / 2,
      })
      return rectFromTwoPoints(newCorner1, newCorner2)
    }
  }
}

function getFixedEdgePositionForAspectRatioLockResize(
  centerBased: IsCenterBased,
  newCorner: CanvasPoint,
  oppositeCorner: CanvasPoint,
): EdgePosition {
  if (centerBased === 'center-based') {
    return { x: 0.5, y: 0.5 }
  } else {
    if (oppositeCorner.x < newCorner.x) {
      if (oppositeCorner.y < newCorner.y) {
        return { x: 0, y: 0 }
      } else {
        return { x: 0, y: 1 }
      }
    } else {
      if (oppositeCorner.y < newCorner.y) {
        return { x: 1, y: 0 }
      } else {
        return { x: 1, y: 1 }
      }
    }
  }
}

function extendRectangleToAspectRatio(
  rectangle: CanvasRectangle,
  fixedEdgePosition: EdgePosition,
  aspectRatio: number,
): CanvasRectangle {
  const currentAspectRatio = rectangle.width / rectangle.height
  if (currentAspectRatio < aspectRatio) {
    const newWidth = rectangle.height * aspectRatio
    if (fixedEdgePosition.x === 0) {
      return canvasRectangle({
        x: rectangle.x,
        y: rectangle.y,
        width: newWidth,
        height: rectangle.height,
      })
    } else if (fixedEdgePosition.x === 0.5) {
      return canvasRectangle({
        x: rectangle.x + (rectangle.width - newWidth) / 2,
        y: rectangle.y,
        width: newWidth,
        height: rectangle.height,
      })
    } else {
      return canvasRectangle({
        x: rectangle.x + rectangle.width - newWidth,
        y: rectangle.y,
        width: newWidth,
        height: rectangle.height,
      })
    }
  } else {
    const newHeight = rectangle.width / aspectRatio
    if (fixedEdgePosition.y === 0) {
      return canvasRectangle({
        x: rectangle.x,
        y: rectangle.y,
        width: rectangle.width,
        height: newHeight,
      })
    } else if (fixedEdgePosition.y === 0.5) {
      return canvasRectangle({
        x: rectangle.x,
        y: rectangle.y + (rectangle.height - newHeight) / 2,
        width: rectangle.width,
        height: newHeight,
      })
    } else {
      return canvasRectangle({
        x: rectangle.x,
        y: rectangle.y + rectangle.height - newHeight,
        width: rectangle.width,
        height: newHeight,
      })
    }
  }
}
