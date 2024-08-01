import type {
  CanvasRectangle,
  LocalRectangle,
  SimpleRectangle,
  Size,
} from '../../../../core/shared/math-utils'
import { canvasRectangle } from '../../../../core/shared/math-utils'
import { roundToNearestWhole } from '../../../../core/shared/math-utils'

type FramePoint = keyof FrameWithAllPoints

const AllFramePoints = ['left', 'top', 'right', 'bottom', 'width', 'height'] as const
const HorizontalFramePoints = ['left', 'right', 'width'] as const
const VerticalFramePoints = ['top', 'bottom', 'height'] as const

function isHorizontalPoint(point: FramePoint): boolean {
  return (HorizontalFramePoints as ReadonlyArray<FramePoint>).includes(point)
}
function isVerticalPoint(point: FramePoint): boolean {
  return (VerticalFramePoints as ReadonlyArray<FramePoint>).includes(point)
}

export type FrameWithAllPoints = {
  left: number
  top: number
  right: number
  bottom: number
  width: number
  height: number
}

export function rectangleToSixFramePoints(
  rectangle: SimpleRectangle,
  containerSize: Size,
): FrameWithAllPoints {
  return {
    left: rectangle.x,
    right: containerSize.width - (rectangle.x + rectangle.width),
    width: rectangle.width,
    top: rectangle.y,
    bottom: containerSize.height - (rectangle.y + rectangle.height),
    height: rectangle.height,
  }
}

export type FrameWithAllPointsSomeOptional = {
  left: number
  top: number
  right: number | null
  bottom: number | null
  width: number
  height: number
}

export function rectangleToSixFramePointsOptionalContainer(
  rectangle: SimpleRectangle,
  containerSize: Size | null,
): FrameWithAllPointsSomeOptional {
  return {
    left: rectangle.x,
    right: containerSize == null ? null : containerSize.width - (rectangle.x + rectangle.width),
    width: rectangle.width,
    top: rectangle.y,
    bottom: containerSize == null ? null : containerSize.height - (rectangle.y + rectangle.height),
    height: rectangle.height,
  }
}

export function sixFramePointsToCanvasRectangle(
  frameWithAllPoints: FrameWithAllPoints,
): CanvasRectangle {
  return canvasRectangle({
    x: frameWithAllPoints.left,
    y: frameWithAllPoints.top,
    width: frameWithAllPoints.width,
    height: frameWithAllPoints.height,
  })
}

export function roundSixPointFrameToNearestWhole(
  fullFrame: FrameWithAllPoints,
): FrameWithAllPoints {
  return {
    left: roundToNearestWhole(fullFrame.left),
    top: roundToNearestWhole(fullFrame.top),
    right: roundToNearestWhole(fullFrame.right),
    bottom: roundToNearestWhole(fullFrame.bottom),
    width: roundToNearestWhole(fullFrame.width),
    height: roundToNearestWhole(fullFrame.height),
  }
}

export function transformConstrainedLocalFullFrameUsingBoundingBox(
  groupOriginalBoundingBox: CanvasRectangle,
  groupNewBoundingBox: CanvasRectangle,
  childrenBoundingBox: CanvasRectangle,
  currentFrame: FrameWithAllPoints,
  constrainedPoints: Array<FramePoint>,
): FrameWithAllPoints {
  function sumConstrainedLengths(sum: number, framePoint: FramePoint) {
    return sum + currentFrame[framePoint]
  }

  const horizontalConstrainedPoints = constrainedPoints.filter(isHorizontalPoint)
  const verticalContrainedPoints = constrainedPoints.filter(isVerticalPoint)

  // first we sum the lengths of all constrained frame points for each dimension
  const horizontalConstrainedLength: number = horizontalConstrainedPoints.reduce(
    sumConstrainedLengths,
    0,
  )
  const verticalConstrainedLength: number = verticalContrainedPoints.reduce(
    sumConstrainedLengths,
    0,
  )

  const updatedFrame: FrameWithAllPoints = AllFramePoints.reduce(
    (newFullFrame: Partial<FrameWithAllPoints>, framePoint) => {
      const horizontal = isHorizontalPoint(framePoint)

      const currentSizeForDimension = horizontal
        ? childrenBoundingBox.width
        : childrenBoundingBox.height

      const newSizeForDimension = horizontal
        ? groupNewBoundingBox.width
        : groupNewBoundingBox.height

      const constrainedPointsForDimension = horizontal
        ? horizontalConstrainedPoints
        : verticalContrainedPoints

      // if this is the only unconstrained point for the dimension, let's just give it all the extra size to avoid a multiply by zero later down the line
      if (
        constrainedPointsForDimension.length === 2 &&
        !constrainedPointsForDimension.includes(framePoint)
      ) {
        newFullFrame[framePoint] =
          currentFrame[framePoint] + newSizeForDimension - currentSizeForDimension
        return newFullFrame
      }

      // a zero-length Frame Point is the equivalent of a constrained one. I include it here to avoid a division by zero later down the line
      if (constrainedPoints.includes(framePoint) || currentFrame[framePoint] === 0) {
        // for Constrained Frame Points, we simply return the current value
        newFullFrame[framePoint] = currentFrame[framePoint]
        return newFullFrame
      }

      const constrainedLengthForDimension = horizontal
        ? horizontalConstrainedLength
        : verticalConstrainedLength

      const currentFramePointLength = currentFrame[framePoint]

      // For non-constrained Frame Points, we change them by calculating the ratio of the
      // old bounding rectangle's non-constrained area vs the new bounding rectangle's non-constrained area.
      const dimensionRatioDivisor = currentSizeForDimension - constrainedLengthForDimension
      const dimensionRatio =
        dimensionRatioDivisor === 0
          ? 1
          : (newSizeForDimension - constrainedLengthForDimension) / dimensionRatioDivisor

      function getUpdatedFramePoint(): number {
        switch (framePoint) {
          case 'left':
            const leftShift = groupOriginalBoundingBox.x - childrenBoundingBox.x
            return currentFramePointLength + leftShift
          case 'top':
            const topShift = groupOriginalBoundingBox.y - childrenBoundingBox.y
            return currentFramePointLength + topShift
          default:
            return currentFramePointLength
        }
      }

      const updatedFramePointValue = getUpdatedFramePoint()

      newFullFrame[framePoint] = updatedFramePointValue * dimensionRatio
      return newFullFrame
    },
    {},
  ) as FrameWithAllPoints

  return updatedFrame
}
