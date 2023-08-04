import type { LocalRectangle, Size } from '../../../../core/shared/math-utils'
import { roundToNearestWhole } from '../../../../core/shared/math-utils'

type FramePoint = keyof FrameWithAllPoints

const AllFramePoints = ['left', 'top', 'right', 'bottom', 'width', 'height'] as const
const HorizontalFramePoints = ['left', 'right', 'width'] as const
const VerticalFramePoints = ['top', 'bottom', 'height'] as const

function isHorizontalPoint(point: FramePoint): boolean {
  return (HorizontalFramePoints as ReadonlyArray<FramePoint>).includes(point)
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
    if (constrainedPoints.includes(framePoint)) {
      return sum + currentFrame[framePoint]
    }
    return sum
  }
  // first we sum the lengths of all constrained frame points for each dimension
  const horizontalConstrainedLength: number = HorizontalFramePoints.reduce(sumConstrainedLengths, 0)
  const verticelConstrainedLength: number = VerticalFramePoints.reduce(sumConstrainedLengths, 0)

  const updatedFrame: FrameWithAllPoints = AllFramePoints.reduce(
    (newFullFrame: Partial<FrameWithAllPoints>, framePoint) => {
      // a zero-length Frame Point is the equivalent of a constrained one. I include it here to avoid a division by zero later down the line
      if (constrainedPoints.includes(framePoint) || currentFrame[framePoint] === 0) {
        // for Constrained Frame Points, we simply return the current value
        newFullFrame[framePoint] = currentFrame[framePoint]
        return newFullFrame
      }

      const constrainedLengthForDimension = isHorizontalPoint(framePoint)
        ? horizontalConstrainedLength
        : verticelConstrainedLength
      const currentSizeForDimension = isHorizontalPoint(framePoint)
        ? currentBoundingBox.width
        : currentBoundingBox.height

      const newSizeForDimension = isHorizontalPoint(framePoint)
        ? newBoundingBox.width
        : newBoundingBox.height

      const currentFramePointLength = currentFrame[framePoint]

      // for non-constrained Frame Points, we change them by calculating the ratio of the old bounding rectangle's non-constrained area vs the new bounding rectangle's non-constrained area
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
