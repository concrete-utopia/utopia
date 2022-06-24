import { AllElementProps } from 'src/components/editor/store/editor-state'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  distance,
  lineIntersection,
  offsetPoint,
  pointDifference,
  rectFromTwoPoints,
  sideOfLine,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CSSCursor, EdgePosition } from '../canvas-types'
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
import { GuidelineWithSnappingVector } from '../guideline'
import { AbsolutePin } from './absolute-resize-helpers'

export function createResizeCommands(
  element: JSXElement,
  selectedElement: ElementPath,
  edgePosition: EdgePosition,
  drag: CanvasVector,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  const pins = pinsForEdgePosition(edgePosition)
  return mapDropNulls((pin) => {
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
        'permanent',
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
  if (!lockedAspectRatio) {
    if (isEdgePositionOnSide(edgePosition)) {
      return resizeBoundingBoxFromSide(boundingBox, drag, edgePosition, centerBased)
    } else {
      return resizeBoundingBoxFromCorner(boundingBox, drag, edgePosition, centerBased)
    }
  } else {
    if (isEdgePositionOnSide(edgePosition)) {
      return resizeBoundingBoxFromSideAspectRatioLocked(
        boundingBox,
        drag,
        edgePosition,
        lockedAspectRatio,
        centerBased,
      )
    } else {
      return resizeBoundingBoxFromCornerAspectRatioLocked(
        boundingBox,
        drag,
        edgePosition,
        lockedAspectRatio,
        centerBased,
      )
    }
  }
}

export function resizeBoundingBoxFromCorner(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  centerBased: IsCenterBased,
): CanvasRectangle {
  const startingCornerPosition = {
    x: 1 - edgePosition.x,
    y: 1 - edgePosition.y,
  } as EdgePosition

  const oppositeCorner = pickPointOnRect(boundingBox, startingCornerPosition)
  const draggedCorner = pickPointOnRect(boundingBox, edgePosition)
  const newCorner = offsetPoint(draggedCorner, drag)

  if (centerBased === 'center-based') {
    const oppositeCornerDragged = offsetPoint(
      oppositeCorner,
      pointDifference(drag, zeroCanvasPoint),
    )
    return rectFromTwoPoints(oppositeCornerDragged, newCorner)
  } else {
    return rectFromTwoPoints(oppositeCorner, newCorner)
  }
}

export function resizeBoundingBoxFromCornerAspectRatioLocked(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  lockedAspectRatio: number,
  centerBased: IsCenterBased,
): CanvasRectangle {
  const startingCornerPosition = {
    x: 1 - edgePosition.x,
    y: 1 - edgePosition.y,
  } as EdgePosition

  const oppositeCorner = pickPointOnRect(boundingBox, startingCornerPosition)
  const draggedCorner = pickPointOnRect(boundingBox, edgePosition)
  const newCorner = offsetPoint(draggedCorner, drag)

  // When the aspect ratio is locked, the real new corner is on the horizontal or vertical projection of the mouse position on the diagonal axis of the bounding box
  const horizontalLineB = getPointOnHorizontalLine(newCorner)
  const verticalLineB = getPointOnVerticalLine(newCorner)
  // We could use the oppositeCorner and the draggedCorner to define the diagonal, but that is not safe, because
  // these are the same points in the extreme case when width and height are both 0
  // As a solution we need to generate another point on the diagonal using the draggedCorner and the aspect ratio
  const diagonalA = getPointOnDiagonal(edgePosition, draggedCorner, lockedAspectRatio)
  // We need to decide whether we want horizontal or vertical projection of the dragged corner to the diagonal
  // For top/left and bottom/right corner we need horizontal projection if the dragged corner is on the right of the diagonal
  // For top/right and bottom/left corner we need horizional projection if the dragged corner is on the left side of the diagonal
  const newCornerOnWhichSideOfDiagonal = sideOfLine(diagonalA, draggedCorner, newCorner)
  const isTopLeftOrBottomRightCorner = edgePosition.x === edgePosition.y
  const isHorizontalProjection =
    (isTopLeftOrBottomRightCorner && newCornerOnWhichSideOfDiagonal === 'right') ||
    (!isTopLeftOrBottomRightCorner && newCornerOnWhichSideOfDiagonal === 'left')
  const aspectRatioFixedNewCorner = Utils.forceNotNull(
    'Can not find projection on diagonal for aspect ratio locked resize',
    lineIntersection(
      diagonalA,
      draggedCorner,
      newCorner,
      isHorizontalProjection ? horizontalLineB : verticalLineB,
    ),
  )

  if (centerBased === 'center-based') {
    const oppositeCornerDragged = offsetPoint(
      oppositeCorner,
      pointDifference(aspectRatioFixedNewCorner, draggedCorner),
    )
    return rectFromTwoPoints(oppositeCornerDragged, aspectRatioFixedNewCorner)
  } else {
    return rectFromTwoPoints(oppositeCorner, aspectRatioFixedNewCorner)
  }
}

export function resizeBoundingBoxFromSide(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  centerBased: IsCenterBased,
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
}

export function resizeBoundingBoxFromSideAspectRatioLocked(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
  lockedAspectRatio: number,
  centerBased: IsCenterBased,
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
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
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

function getPointOnHorizontalLine(p: CanvasPoint) {
  return canvasPoint({
    x: p.x + 100,
    y: p.y,
  })
}

// returns a point on the same vertical line as p
function getPointOnVerticalLine(p: CanvasPoint) {
  return canvasPoint({
    x: p.x,
    y: p.y + 100,
  })
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
