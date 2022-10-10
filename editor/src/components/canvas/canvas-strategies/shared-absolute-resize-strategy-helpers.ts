import { AllElementProps } from '../../../components/editor/store/editor-state'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  CanvasVector,
  distance,
  offsetPoint,
  pointDifference,
  rectFromTwoPoints,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CanvasFrameAndTarget, CSSCursor, EdgePosition } from '../canvas-types'
import {
  isEdgePositionAHorizontalEdge,
  isEdgePositionAVerticalEdge,
  isEdgePositionOnSide,
  pickPointOnRect,
  snapPoint,
} from '../canvas-utils'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { pointGuidelineToBoundsEdge } from '../controls/guideline-helpers'
import { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../guideline'
import { AbsolutePin } from './resize-helpers'

export function createResizeCommands(
  element: JSXElement,
  selectedElement: ElementPath,
  edgePosition: EdgePosition,
  drag: CanvasVector,
  elementGlobalFrame: CanvasRectangle | null,
  elementParentBounds: CanvasRectangle | null,
): { commands: AdjustCssLengthProperty[]; intendedBounds: CanvasFrameAndTarget | null } {
  const pins = pinsForEdgePosition(edgePosition)
  const commands = mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const negative =
      pin === 'right' ||
      pin === 'bottom' ||
      (pin === 'width' && edgePosition.x === 0) ||
      (pin === 'height' && edgePosition.y === 0)
    const value = getLayoutProperty(pin, right(element.props), ['style'])
    if (isRight(value) && value.value != null) {
      // TODO what to do about missing properties?
      return adjustCssLengthProperty(
        'always',
        selectedElement,
        stylePropPathMappingFn(pin, ['style']),
        (horizontal ? drag.x : drag.y) * (negative ? -1 : 1),
        horizontal ? elementParentBounds?.width : elementParentBounds?.height,
        true,
      )
    } else {
      return null
    }
  }, pins)

  const intendedBounds: CanvasFrameAndTarget | null =
    elementGlobalFrame == null
      ? null
      : {
          frame: resizeBoundingBox(
            elementGlobalFrame,
            drag,
            edgePosition,
            null,
            'non-center-based',
          ),
          target: selectedElement,
        }
  return { commands, intendedBounds }
}

function pinsForEdgePosition(edgePosition: EdgePosition): AbsolutePin[] {
  let horizontalPins: AbsolutePin[] = []
  let verticalPins: AbsolutePin[] = []

  if (edgePosition.x === 0) {
    horizontalPins = ['left', 'width']
  } else if (edgePosition.x === 1) {
    horizontalPins = ['right', 'width']
  }

  if (edgePosition.y === 0) {
    verticalPins = ['top', 'height']
  } else if (edgePosition.y === 1) {
    verticalPins = ['bottom', 'height']
  }

  return [...horizontalPins, ...verticalPins]
}

type IsCenterBased = 'center-based' | 'non-center-based'

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

export function runLegacyAbsoluteResizeSnapping(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  draggedCorner: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: IsCenterBased,
  allElementProps: AllElementProps,
): {
  snapDelta: CanvasVector
  snappedBoundingBox: CanvasRectangle
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  const oppositeCorner: EdgePosition = {
    x: 1 - draggedCorner.x,
    y: 1 - draggedCorner.y,
  } as EdgePosition

  const draggedPointMovedWithoutSnap = pickPointOnRect(resizedBounds, draggedCorner)
  const oppositePoint =
    lockedAspectRatio == null
      ? pickPointOnRect(resizedBounds, oppositeCorner)
      : getPointOnDiagonal(draggedCorner, draggedPointMovedWithoutSnap, lockedAspectRatio)

  const { snappedPointOnCanvas, guidelinesWithSnappingVector } = snapPoint(
    selectedElements,
    jsxMetadata,
    canvasScale,
    draggedPointMovedWithoutSnap,
    true,
    lockedAspectRatio != null,
    draggedPointMovedWithoutSnap,
    oppositePoint,
    draggedCorner,
    allElementProps,
  )

  const snapDelta = pointDifference(draggedPointMovedWithoutSnap, snappedPointOnCanvas)
  const snappedBounds = resizeBoundingBox(
    resizedBounds,
    snapDelta,
    draggedCorner,
    lockedAspectRatio,
    centerBased,
  )

  const updatedGuidelinesWithSnapping = pointGuidelineToBoundsEdge(
    guidelinesWithSnappingVector,
    snappedBounds,
  )

  return {
    snapDelta: snapDelta,
    snappedBoundingBox: snappedBounds,
    guidelinesWithSnappingVector: updatedGuidelinesWithSnapping,
  }
}

// returns a point on the diagonal of a rectangle, using a given corner and the aspect ratio of the rectangle, in the direction towards the inside of the rectangle
// from the center of the sides it returns a point on the orthogonal line from the side center
function getPointOnDiagonal(
  edgePosition: EdgePosition,
  cornerPoint: CanvasPoint,
  aspectRatio: number,
) {
  if (edgePosition.x === 0 && edgePosition.y === 0) {
    return canvasPoint({
      x: cornerPoint.x + 100 * aspectRatio,
      y: cornerPoint.y + 100,
    })
  }
  if (edgePosition.x === 0 && edgePosition.y === 1) {
    return canvasPoint({
      x: cornerPoint.x + 100 * aspectRatio,
      y: cornerPoint.y - 100,
    })
  }
  if (edgePosition.x === 1 && edgePosition.y === 1) {
    return canvasPoint({
      x: cornerPoint.x - 100 * aspectRatio,
      y: cornerPoint.y - 100,
    })
  }
  if (edgePosition.x === 1 && edgePosition.y === 0) {
    return canvasPoint({
      x: cornerPoint.x - 100 * aspectRatio,
      y: cornerPoint.y + 100,
    })
  }
  if (edgePosition.x === 0.5 && edgePosition.y === 0) {
    return canvasPoint({
      x: cornerPoint.x,
      y: cornerPoint.y + 100,
    })
  }
  if (edgePosition.x === 0.5 && edgePosition.y === 1) {
    return canvasPoint({
      x: cornerPoint.x,
      y: cornerPoint.y - 100,
    })
  }
  if (edgePosition.x === 0 && edgePosition.y === 0.5) {
    return canvasPoint({
      x: cornerPoint.x + 100,
      y: cornerPoint.y,
    })
  }
  if (edgePosition.x === 1 && edgePosition.y === 0.5) {
    return canvasPoint({
      x: cornerPoint.x + 100,
      y: cornerPoint.y,
    })
  }
  throw new Error(`Edge position ${edgePosition} is not a corner position`)
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
