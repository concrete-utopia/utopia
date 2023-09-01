import type { LocalRectangle, Size } from '../../../../core/shared/math-utils'
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

export function localRectangleToSixFramePoints(
  localRectangle: LocalRectangle,
  containerSize: Size,
): FrameWithAllPoints {
  return {
    left: localRectangle.x,
    right: containerSize.width - (localRectangle.x + localRectangle.width),
    width: localRectangle.width,
    top: localRectangle.y,
    bottom: containerSize.height - (localRectangle.y + localRectangle.height),
    height: localRectangle.height,
  }
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
  newBoundingBox: Size,
  currentBoundingBox: Size,
  currentFrame: FrameWithAllPoints,
  constrainedPoints: Array<FramePoint>,
): FrameWithAllPoints {
  // TODO verify that currentBoundingBox and currentFrame's dimensions match up

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
        ? currentBoundingBox.width
        : currentBoundingBox.height

      const newSizeForDimension = horizontal ? newBoundingBox.width : newBoundingBox.height

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

      // for non-constrained Frame Points, we change them by calculating the ratio of the old bounding rectangle's non-constrained area vs the new bounding rectangle's non-constrained area
      // as you can see above, while I think the maths is right, zeros are creeping in causing edge cases I need to fix
      // I think this function could be rewritten into a much shorter, way more elegant piece of code
      const updatedFramePointLength =
        (currentFramePointLength * (newSizeForDimension - constrainedLengthForDimension)) /
        (currentSizeForDimension - constrainedLengthForDimension)

      newFullFrame[framePoint] = updatedFramePointLength
      return newFullFrame
    },
    {},
  ) as FrameWithAllPoints

  return updatedFrame
}
