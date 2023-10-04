import type { Axis, DiagonalAxis } from '../../utils/utils'
import Utils from '../../utils/utils'
import type {
  CanvasPoint,
  CanvasVector,
  CoordinateMarker,
  Point,
  Rectangle,
  Vector,
  LocalRectangle,
  CanvasRectangle,
} from '../../core/shared/math-utils'
import { canvasPoint, roundToNearestWhole } from '../../core/shared/math-utils'

export interface XAxisGuideline {
  type: 'XAxisGuideline'
  x: number
  yTop: number
  yBottom: number
}

export interface YAxisGuideline {
  type: 'YAxisGuideline'
  y: number
  xLeft: number
  xRight: number
}

export interface CornerGuideline {
  type: 'CornerGuideline'
  x: number
  y: number
  xMovement: number
  yMovement: number
}

export type Guideline = XAxisGuideline | YAxisGuideline | CornerGuideline

export interface GuidelineWithRelevantPoints {
  guideline: Guideline
  pointsOfRelevance: Array<CanvasPoint>
}

export function xAxisGuideline(x: number, yTop: number, yBottom: number): XAxisGuideline {
  return {
    type: 'XAxisGuideline',
    x: x,
    yTop: yTop,
    yBottom: yBottom,
  }
}

export function yAxisGuideline(y: number, xLeft: number, xRight: number): YAxisGuideline {
  return {
    type: 'YAxisGuideline',
    y: y,
    xLeft: xLeft,
    xRight: xRight,
  }
}

export function cornerGuideline(
  x: number,
  y: number,
  xMovement: number,
  yMovement: number,
): CornerGuideline {
  return {
    type: 'CornerGuideline',
    x: x,
    y: y,
    xMovement: xMovement,
    yMovement: yMovement,
  }
}

export interface GuidelineWithSnappingVectorAndPointsOfRelevance {
  guideline: Guideline
  snappingVector: CanvasVector
  pointsOfRelevance: Array<CanvasPoint>
}

export function guidelineWithSnappingVectorAndPointsOfRelevance(
  guideline: Guideline,
  snappingVector: CanvasVector,
  pointsOfRelevance: Array<CanvasPoint>,
): GuidelineWithSnappingVectorAndPointsOfRelevance {
  return {
    guideline: guideline,
    snappingVector: snappingVector,
    pointsOfRelevance: pointsOfRelevance,
  }
}

export type ConstrainedDragAxis = Axis | DiagonalAxis

function constrainedIsDiagonal(
  constrainedDragAxis: ConstrainedDragAxis | null,
): constrainedDragAxis is DiagonalAxis {
  switch (constrainedDragAxis) {
    case 'BottomLeftToTopRight':
    case 'TopLeftToBottomRight':
      return true
    default:
      return false
  }
}

function minDistance(ds: Array<number>): number {
  return ds.reduce((currentMin, dist) => {
    return Math.abs(dist) < currentMin ? dist : currentMin
  }, Number.MAX_VALUE)
}

function applyDirectionConstraint(
  constrainedDragAxis: ConstrainedDragAxis | null,
  drag: CanvasPoint,
): CanvasPoint {
  const xIsLarger = Math.abs(drag.x) > Math.abs(drag.y)
  switch (constrainedDragAxis) {
    case null:
      return drag
    case 'x':
      return { x: drag.x, y: 0 } as CanvasPoint
    case 'y':
      return { x: 0, y: drag.y } as CanvasPoint
    case 'BottomLeftToTopRight':
      return {
        x: xIsLarger ? drag.x : -drag.y,
        y: xIsLarger ? -drag.x : drag.y,
      } as CanvasPoint
    case 'TopLeftToBottomRight':
      return {
        x: xIsLarger ? drag.x : drag.y,
        y: xIsLarger ? drag.x : drag.y,
      } as CanvasPoint
    default:
      const _exhaustiveCheck: never = constrainedDragAxis
      throw new Error(`Unhandled axis ${JSON.stringify(constrainedDragAxis)}`)
  }
}

interface EdgeGuidelinePoints {
  type: 'edgeguidelinepoints'
  start: CanvasPoint
  end: CanvasPoint
}

function edgeGuidelinePoints(start: CanvasPoint, end: CanvasPoint): EdgeGuidelinePoints {
  return {
    type: 'edgeguidelinepoints',
    start: start,
    end: end,
  }
}

interface CornerGuidelinePoint {
  type: 'cornerguidelinepoint'
  point: CanvasPoint
}

function cornerGuidelinePoint(point: CanvasPoint): CornerGuidelinePoint {
  return {
    type: 'cornerguidelinepoint',
    point: point,
  }
}

type GuidelinePoints = EdgeGuidelinePoints | CornerGuidelinePoint

function pointsForGuidelinesForFrame(
  frame: LocalRectangle | CanvasRectangle,
  includeCentre: boolean,
): {
  xs: Array<number>
  ys: Array<number>
  xLeft: number
  xRight: number
  yTop: number
  yBottom: number
} {
  const xLeft = roundToNearestWhole(frame.x)
  const xCentre = roundToNearestWhole(frame.x + frame.width / 2)
  const xRight = roundToNearestWhole(frame.x + frame.width)
  const yTop = roundToNearestWhole(frame.y)
  const yCentre = roundToNearestWhole(frame.y + frame.height / 2)
  const yBottom = roundToNearestWhole(frame.y + frame.height)
  const xs = includeCentre ? [xLeft, xCentre, xRight] : [xLeft, xRight]
  const ys = includeCentre ? [yTop, yCentre, yBottom] : [yTop, yBottom]

  return {
    xs: xs,
    ys: ys,
    xLeft: xLeft,
    xRight: xRight,
    yTop: yTop,
    yBottom: yBottom,
  }
}

export const Guidelines = {
  applyDirectionConstraint: applyDirectionConstraint,
  convertGuidelineToPoints: function (guideline: Guideline): GuidelinePoints {
    switch (guideline.type) {
      case 'XAxisGuideline':
        return edgeGuidelinePoints(
          {
            x: guideline.x,
            y: guideline.yTop,
          } as CanvasPoint,
          {
            x: guideline.x,
            y: guideline.yBottom,
          } as CanvasPoint,
        )
      case 'YAxisGuideline':
        return edgeGuidelinePoints(
          {
            x: guideline.xLeft,
            y: guideline.y,
          } as CanvasPoint,
          {
            x: guideline.xRight,
            y: guideline.y,
          } as CanvasPoint,
        )
      case 'CornerGuideline':
        return cornerGuidelinePoint(Utils.point(guideline.x, guideline.y))
      default:
        const _exhaustiveCheck: never = guideline
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type
    }
  },
  xAxisGuideline: xAxisGuideline,
  yAxisGuideline: yAxisGuideline,
  cornerGuideline: cornerGuideline,
  applySnappingToDrag(
    drag: CanvasPoint,
    dragResult: CanvasPoint,
    xOffsets: Array<number>,
    yOffsets: Array<number>,
    guidelines: Array<Guideline>,
    snappingThreshold: number,
    constrainedDragAxis: ConstrainedDragAxis | null,
  ): CanvasPoint {
    // FIXME RJB Kill remaining frame controls from the old canvas controls
    console.error('This code path should be dead')
    return dragResult
  },
  guidelinesForFrame: function (
    frame: LocalRectangle | CanvasRectangle,
    includeCentre: boolean,
  ): Array<Guideline> {
    const { xs, ys, xLeft, xRight, yTop, yBottom } = pointsForGuidelinesForFrame(
      frame,
      includeCentre,
    )
    const xGuidelines = xs.map((x) => {
      return Guidelines.xAxisGuideline(x, yTop, yBottom)
    })
    const yGuidelines = ys.map((y) => {
      return Guidelines.yAxisGuideline(y, xLeft, xRight)
    })

    return [...xGuidelines, ...yGuidelines]
  },
  guidelinesWithRelevantPointsForFrame: function (
    frame: LocalRectangle | CanvasRectangle,
    includeCentre: 'include' | 'exclude',
  ): Array<GuidelineWithRelevantPoints> {
    const { xs, ys, xLeft, xRight, yTop, yBottom } = pointsForGuidelinesForFrame(
      frame,
      includeCentre === 'include',
    )

    const xGuidelines: Array<GuidelineWithRelevantPoints> = xs.map((x) => {
      return {
        guideline: Guidelines.xAxisGuideline(x, yTop, yBottom),
        pointsOfRelevance: [canvasPoint({ x: x, y: yTop }), canvasPoint({ x: x, y: yBottom })],
      }
    })

    const yGuidelines: Array<GuidelineWithRelevantPoints> = ys.map((y) => {
      return {
        guideline: Guidelines.yAxisGuideline(y, xLeft, xRight),
        pointsOfRelevance: [canvasPoint({ x: xLeft, y: y }), canvasPoint({ x: xRight, y: y })],
      }
    })

    return [...xGuidelines, ...yGuidelines]
  },
  distanceFromPointToGuideline: function <C extends CoordinateMarker>(
    point: Point<C>,
    guideline: Guideline,
  ): { distance: number; from: Point<C>; to: Point<C> } {
    switch (guideline.type) {
      case 'XAxisGuideline':
        return {
          distance: Math.abs(guideline.x - point.x),
          from: point as Point<C>,
          to: { x: guideline.x, y: point.y } as Point<C>,
        }
      case 'YAxisGuideline':
        return {
          distance: Math.abs(guideline.y - point.y),
          from: point as Point<C>,
          to: { x: point.x, y: guideline.y } as Point<C>,
        }
      case 'CornerGuideline':
        const guidelinePoint: Point<C> = Utils.point(guideline.x, guideline.y)
        return {
          distance: Utils.distance(guidelinePoint, point),
          from: point as Point<C>,
          to: guidelinePoint,
        }
      default: // TODO ts-migration
        const _exhaustiveCheck: never = guideline
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type // TODO ts-migration
    }
  },
  distanceFromFrameToGuideline: function <C extends CoordinateMarker>(
    frame: Rectangle<C>,
    guideline: Guideline,
    includeCentre: boolean,
  ): { distance: number; from: Point<C>; to: Point<C> } {
    const midX = frame.x + frame.width / 2
    const midY = frame.y + frame.height / 2
    switch (guideline.type) {
      case 'XAxisGuideline':
        const xs = includeCentre
          ? [guideline.x - frame.x, guideline.x - midX, guideline.x - (frame.x + frame.width)]
          : [guideline.x - frame.x, guideline.x - (frame.x + frame.width)]
        const xDistance = minDistance(xs)

        return {
          distance: Math.abs(xDistance),
          from: { x: guideline.x - xDistance, y: midY } as Point<C>,
          to: { x: guideline.x, y: midY } as Point<C>,
        }
      case 'YAxisGuideline':
        const ys = includeCentre
          ? [guideline.y - frame.y, guideline.y - midY, guideline.y - (frame.y + frame.height)]
          : [guideline.y - frame.y, guideline.y - (frame.y + frame.height)]
        const yDistance = minDistance(ys)

        return {
          distance: Math.abs(yDistance),
          from: { x: midX, y: guideline.y - yDistance } as Point<C>,
          to: { x: midX, y: guideline.y } as Point<C>,
        }
      case 'CornerGuideline':
        const cornerPoint: Point<C> = Utils.point(guideline.x, guideline.y)
        let [nearestCorner, ...remainingCorners] = Utils.rectangleToPoints(frame)
        let cornerDistance = Utils.distance(nearestCorner, cornerPoint)
        Utils.fastForEach(remainingCorners, (remainingCorner) => {
          const remainingCornerDistance = Utils.distance(remainingCorner, cornerPoint)
          if (remainingCornerDistance < cornerDistance) {
            cornerDistance = remainingCornerDistance
            nearestCorner = remainingCorner
          }
        })
        return {
          distance: cornerDistance,
          from: nearestCorner,
          to: cornerPoint,
        }
      default:
        const _exhaustiveCheck: never = guideline
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type
    }
  },
  getOffsetToSnapToXGuideline: function (
    xs: Array<number>,
    guideline: XAxisGuideline,
    constrainedDragAxis: ConstrainedDragAxis | null,
  ): Vector<any> {
    const distances = xs.map((x) => {
      return guideline.x - x
    })
    const mininum = minDistance(distances)
    if (constrainedIsDiagonal(constrainedDragAxis)) {
      switch (constrainedDragAxis) {
        case 'BottomLeftToTopRight':
          return { x: mininum, y: -mininum }
        case 'TopLeftToBottomRight':
          return { x: mininum, y: mininum }
        default:
          const _exhaustiveCheck: never = constrainedDragAxis
          throw new Error(`Unhandled diagonal axis: ${JSON.stringify(constrainedDragAxis)}`)
      }
    } else {
      return { x: mininum, y: 0 }
    }
  },
  getOffsetToSnapToYGuideline: function (
    ys: Array<number>,
    guideline: YAxisGuideline,
    constrainedDragAxis: ConstrainedDragAxis | null,
  ): Vector<any> {
    const distances = ys.map((y) => {
      return guideline.y - y
    })
    const mininum = minDistance(distances)
    if (constrainedIsDiagonal(constrainedDragAxis)) {
      switch (constrainedDragAxis) {
        case 'BottomLeftToTopRight':
          return { x: -mininum, y: mininum }
        case 'TopLeftToBottomRight':
          return { x: mininum, y: mininum }
        default:
          const _exhaustiveCheck: never = constrainedDragAxis
          throw new Error(`Unhandled diagonal axis: ${JSON.stringify(constrainedDragAxis)}`)
      }
    } else {
      return { x: 0, y: mininum }
    }
  },
  getOffsetToSnapToCornerGuideline: function (
    corners: Array<Point<any>>,
    guideline: CornerGuideline,
  ): Vector<any> {
    const guidelinePoint = Utils.point(guideline.x, guideline.y)
    let [nearestCorner, ...otherCorners] = corners
    let nearestOffset = Utils.pointDifference(nearestCorner, guidelinePoint)
    let nearestMagnitude = Utils.magnitude(nearestOffset)
    Utils.fastForEach(otherCorners, (otherCorner) => {
      const cornerOffset = Utils.pointDifference(otherCorner, guidelinePoint)
      const cornerOffsetMagnitude = Utils.magnitude(cornerOffset)
      if (cornerOffsetMagnitude < nearestMagnitude) {
        nearestOffset = cornerOffset
        nearestMagnitude = cornerOffsetMagnitude
      }
    })
    return nearestOffset
  },
  getOffsetToSnapToGuideline: function (
    xs: Array<number>,
    ys: Array<number>,
    corners: Array<Point<any>>,
    guideline: Guideline,
    constrainedDragAxis: ConstrainedDragAxis | null,
  ): Vector<any> {
    switch (guideline.type) {
      case 'XAxisGuideline':
        return Guidelines.getOffsetToSnapToXGuideline(xs, guideline, constrainedDragAxis)
      case 'YAxisGuideline':
        return Guidelines.getOffsetToSnapToYGuideline(ys, guideline, constrainedDragAxis)
      case 'CornerGuideline':
        return Guidelines.getOffsetToSnapToCornerGuideline(corners, guideline)
      default: // TODO ts-migration
        const _exhaustiveCheck: never = guideline
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type // TODO ts-migration
    }
  },
  newSnappingVectorIsSmallest<C extends CoordinateMarker>(l: Point<C>, r: Point<C>): boolean {
    return Math.abs(Utils.magnitude(l)) < Math.abs(Utils.magnitude(r))
  },
  newSnappingVectorIsEqual<C extends CoordinateMarker>(l: Point<C>, r: Point<C>): boolean {
    return Utils.magnitude(l) === Utils.magnitude(r)
  },
  shouldSnap(snappingVector: Vector<any>, snappingThreshold: number, scale: number): boolean {
    return Utils.magnitude(snappingVector) < snappingThreshold / scale
  },
  getClosestGuidelinesAndOffsets(
    xs: Array<number>,
    ys: Array<number>,
    corners: Array<CanvasPoint>,
    guidelines: Array<GuidelineWithRelevantPoints>,
    constrainedDragAxis: ConstrainedDragAxis | null,
    snappingThreshold: number,
    scale: number,
  ): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
    let xGuidelinesAndOffsets: Array<GuidelineWithSnappingVectorAndPointsOfRelevance> = []
    let yGuidelinesAndOffsets: Array<GuidelineWithSnappingVectorAndPointsOfRelevance> = []
    for (const { guideline, pointsOfRelevance } of guidelines) {
      if (guideline.type === 'XAxisGuideline' && constrainedDragAxis !== 'y') {
        const snappingVector = Guidelines.getOffsetToSnapToXGuideline(
          xs,
          guideline,
          constrainedDragAxis,
        )
        const activateSnap = this.shouldSnap(snappingVector, snappingThreshold, scale)
        if (activateSnap) {
          const guidelineAndOffset: GuidelineWithSnappingVectorAndPointsOfRelevance = {
            guideline: guideline,
            snappingVector: snappingVector,
            pointsOfRelevance,
          }

          if (
            xGuidelinesAndOffsets.length === 0 ||
            Guidelines.newSnappingVectorIsSmallest(
              snappingVector,
              xGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            xGuidelinesAndOffsets = [guidelineAndOffset]
          } else if (
            Guidelines.newSnappingVectorIsEqual(
              snappingVector,
              xGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            xGuidelinesAndOffsets.push(guidelineAndOffset)
          }
        }
      } else if (guideline.type === 'YAxisGuideline' && constrainedDragAxis !== 'x') {
        const snappingVector = Guidelines.getOffsetToSnapToYGuideline(
          ys,
          guideline,
          constrainedDragAxis,
        )
        const activateSnap = this.shouldSnap(snappingVector, snappingThreshold, scale)
        if (activateSnap) {
          const guidelineAndOffset: GuidelineWithSnappingVectorAndPointsOfRelevance = {
            guideline: guideline,
            snappingVector: snappingVector,
            pointsOfRelevance,
          }

          if (
            yGuidelinesAndOffsets.length === 0 ||
            Guidelines.newSnappingVectorIsSmallest(
              snappingVector,
              yGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            yGuidelinesAndOffsets = [guidelineAndOffset]
          } else if (
            Guidelines.newSnappingVectorIsEqual(
              snappingVector,
              yGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            yGuidelinesAndOffsets.push(guidelineAndOffset)
          }
        }
      }
    }
    Utils.fastForEach(guidelines, ({ guideline, pointsOfRelevance }) => {
      if (guideline.type === 'CornerGuideline') {
        const snappingVector = Guidelines.getOffsetToSnapToCornerGuideline(corners, guideline)
        const activateSnap = this.shouldSnap(snappingVector, snappingThreshold, scale)
        if (activateSnap) {
          const guidelineAndOffset: GuidelineWithSnappingVectorAndPointsOfRelevance = {
            guideline: guideline,
            snappingVector: snappingVector,
            pointsOfRelevance,
          }

          if (
            xGuidelinesAndOffsets.length === 0 ||
            Guidelines.newSnappingVectorIsSmallest(
              snappingVector,
              xGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            xGuidelinesAndOffsets = [guidelineAndOffset]
          } else if (
            Guidelines.newSnappingVectorIsEqual(
              snappingVector,
              xGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            xGuidelinesAndOffsets.push(guidelineAndOffset)
          }
          if (
            yGuidelinesAndOffsets.length === 0 ||
            Guidelines.newSnappingVectorIsSmallest(
              snappingVector,
              yGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            yGuidelinesAndOffsets = [guidelineAndOffset]
          } else if (
            Guidelines.newSnappingVectorIsEqual(
              snappingVector,
              yGuidelinesAndOffsets[0].snappingVector,
            )
          ) {
            yGuidelinesAndOffsets.push(guidelineAndOffset)
          }
        }
      }
    })

    const guidelineResults = [...xGuidelinesAndOffsets, ...yGuidelinesAndOffsets]

    return guidelineResults.filter(
      (g) => Utils.magnitude(g.snappingVector) < (snappingThreshold / scale) * 2,
    )
  },
  offsetGuideline: function (guideline: Guideline, offset: Point<any>): Guideline {
    switch (guideline.type) {
      case 'XAxisGuideline':
        return xAxisGuideline(
          guideline.x + offset.x,
          guideline.yTop + offset.y,
          guideline.yBottom + offset.y,
        )
      case 'YAxisGuideline':
        return yAxisGuideline(
          guideline.y + offset.y,
          guideline.xLeft + offset.x,
          guideline.xRight + offset.x,
        )
      case 'CornerGuideline':
        return cornerGuideline(
          guideline.x + offset.x,
          guideline.y + offset.y,
          guideline.xMovement + offset.x,
          guideline.yMovement + offset.y,
        )
      default: // TODO ts-migration
        const _exhaustiveCheck: never = guideline
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type // TODO ts-migration
    }
  },
  guidelineIntersection: function (guidelines: Array<Guideline>): CanvasPoint | null {
    if (guidelines.length === 2) {
      const maybePoint = guidelines.reduce((point, guideline) => {
        if (guideline.type === 'XAxisGuideline') {
          return {
            ...point,
            x: guideline.x,
          }
        } else {
          return {
            ...point,
            y: guideline.y,
          }
        }
      }, {} as Partial<CanvasPoint>)
      if (maybePoint.x != null && maybePoint.y != null) {
        return maybePoint as CanvasPoint
      }
    }
    return null
  },
}
