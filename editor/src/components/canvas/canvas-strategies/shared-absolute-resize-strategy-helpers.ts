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
  closestPointOnLine,
  lineIntersection,
  offsetPoint,
  pointDifference,
  rectFromTwoPoints,
  sideOfLine,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { EdgePosition } from '../canvas-types'
import {
  isEdgePositionAHorizontalEdge,
  isEdgePositionAVerticalEdge,
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
import Utils from '../../../utils/utils'

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
  let dragToUse = drag
  let cornerEdgePosition = edgePosition
  let startingCornerPosition = {
    x: 1 - edgePosition.x,
    y: 1 - edgePosition.y,
  } as EdgePosition

  const isEdgeHorizontalSide = isEdgePositionAHorizontalEdge(edgePosition)
  const isEdgeVerticalSide = isEdgePositionAVerticalEdge(edgePosition)

  if (isEdgeHorizontalSide) {
    dragToUse = canvasPoint({ x: 0, y: drag.y })
    startingCornerPosition = { x: 0, y: startingCornerPosition.y }
    cornerEdgePosition = { x: 1, y: edgePosition.y }
  } else if (isEdgeVerticalSide) {
    dragToUse = canvasPoint({ x: drag.x, y: 0 })
    startingCornerPosition = { x: startingCornerPosition.x, y: 0 }
    cornerEdgePosition = { x: edgePosition.x, y: 1 }
  }

  const oppositeCornerPoint = pickPointOnRect(boundingBox, startingCornerPosition)
  const draggedCorner = pickPointOnRect(boundingBox, cornerEdgePosition)
  let newCorner = offsetPoint(draggedCorner, dragToUse)

  if (lockedAspectRatio != null) {
    const horizontalLineB = canvasPoint({
      x: newCorner.x + 100,
      y: newCorner.y,
    })
    const verticalLineB = canvasPoint({
      x: newCorner.x,
      y: newCorner.y + 100,
    })
    const diagonalA = getPointOnDiagonal(cornerEdgePosition, draggedCorner, lockedAspectRatio)
    if (isEdgeHorizontalSide || isEdgeVerticalSide) {
      newCorner =
        lineIntersection(
          diagonalA,
          draggedCorner,
          newCorner,
          isEdgeHorizontalSide ? horizontalLineB : verticalLineB,
        ) ?? closestPointOnLine(diagonalA, draggedCorner, newCorner)
    } else {
      const pointPosToAxis = sideOfLine(diagonalA, draggedCorner, newCorner)
      const topLeftOrBottomRight = edgePosition.x === edgePosition.y
      const horizontalMatching =
        (topLeftOrBottomRight && pointPosToAxis === 'right') ||
        (!topLeftOrBottomRight && pointPosToAxis === 'left')
      newCorner =
        lineIntersection(
          diagonalA,
          draggedCorner,
          newCorner,
          horizontalMatching ? horizontalLineB : verticalLineB,
        ) ?? closestPointOnLine(diagonalA, draggedCorner, newCorner)
    }
  }

  let newBoundingBox = boundingBox
  if (centerBased === 'center-based') {
    const oppositeCornerDragged = offsetPoint(
      oppositeCornerPoint,
      pointDifference(dragToUse, zeroCanvasPoint),
    )
    newBoundingBox = rectFromTwoPoints(oppositeCornerDragged, newCorner)
  } else {
    newBoundingBox = rectFromTwoPoints(oppositeCornerPoint, newCorner)
  }

  if (lockedAspectRatio != null) {
    if (isEdgeHorizontalSide) {
      newBoundingBox.x = Utils.roundTo(
        boundingBox.x + (boundingBox.width - newBoundingBox.width) / 2,
      )
    } else if (isEdgeVerticalSide) {
      newBoundingBox.y -= Utils.roundTo(
        boundingBox.y + (boundingBox.height - newBoundingBox.height) / 2,
      )
    }
  }

  return newBoundingBox
}

export function runLegacyAbsoluteResizeSnapping(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  draggedCorner: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: IsCenterBased,
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

function getPointOnDiagonal(
  edgePosition: EdgePosition,
  cornerPoint: CanvasPoint,
  aspectRatio: number,
) {
  if (
    (edgePosition.x === 0 && edgePosition.y === 0) ||
    (edgePosition.x === 1 && edgePosition.y === 1) ||
    (edgePosition.x === 0.5 && edgePosition.y === 1) ||
    (edgePosition.x === 1 && edgePosition.y === 0.5)
  ) {
    return canvasPoint({
      x: cornerPoint.x + 100 * aspectRatio,
      y: cornerPoint.y + 100,
    })
  }
  return canvasPoint({
    x: cornerPoint.x + 100 * aspectRatio,
    y: cornerPoint.y - 100,
  })
}
