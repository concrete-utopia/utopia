import * as R from 'ramda'
import Utils, { Axis, DiagonalAxis } from '../../utils/utils'
import {
  CanvasPoint,
  CanvasVector,
  CoordinateMarker,
  Point,
  Rectangle,
  Vector,
  LocalRectangle,
  CanvasRectangle,
} from '../../core/shared/math-utils'

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

type GuidelineWithDistance = {
  guideline: Guideline
  distance: number
}

export type GuidelineWithSnappingVector = {
  guideline: Guideline
  snappingVector: CanvasVector
  activateSnap: boolean
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

export const Guidelines = {
  applyDirectionConstraint: applyDirectionConstraint,
  convertGuidelineToPoints: function(guideline: Guideline): GuidelinePoints {
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
  applySnappingToPoint<C extends CoordinateMarker>(
    point: Point<C>,
    guidelines: Array<Guideline>,
    snappingThreshold: number,
    scale: number,
    constrainedDragAxis: ConstrainedDragAxis | null,
  ): Vector<C> {
    return R.reduce(
      (workingPoint, guideline) => {
        const snappingVector = Guidelines.getOffsetToSnapToGuideline(
          [workingPoint.x],
          [workingPoint.y],
          [],
          guideline,
          constrainedDragAxis,
        )
        const distance = Utils.magnitude(snappingVector)
        if (distance <= snappingThreshold / scale) {
          return Utils.roundPointToNearestHalf(Utils.offsetPoint(workingPoint, snappingVector))
        } else {
          return workingPoint
        }
      },
      point,
      guidelines,
    )
  },
  guidelinesForFrame: function(
    frame: LocalRectangle | CanvasRectangle,
    includeCentre: boolean,
  ): Array<Guideline> {
    const xLeft = frame.x
    const xRight = frame.x + frame.width
    const yTop = frame.y
    const yBottom = frame.y + frame.height
    const xs = includeCentre
      ? [frame.x, frame.x + frame.width / 2, frame.x + frame.width]
      : [frame.x, frame.x + frame.width]
    const ys = includeCentre
      ? [frame.y, frame.y + frame.height / 2, frame.y + frame.height]
      : [frame.y, frame.y + frame.height]
    const xGuidelines = xs.map((x) => {
      return Guidelines.xAxisGuideline(x, yTop, yBottom)
    })
    const yGuidelines = ys.map((y) => {
      return Guidelines.yAxisGuideline(y, xLeft, xRight)
    })

    return [...xGuidelines, ...yGuidelines]
  },
  distanceFromPointToGuideline: function<C extends CoordinateMarker>(
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
      default:
        const _exhaustiveCheck: never = guideline // TODO ts-migration
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type // TODO ts-migration
    }
  },
  distanceFromFrameToGuideline: function<C extends CoordinateMarker>(
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
  getOffsetToSnapToXGuideline: function(
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
  getOffsetToSnapToYGuideline: function(
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
  getOffsetToSnapToCornerGuideline: function(
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
  getOffsetToSnapToGuideline: function(
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
      default:
        const _exhaustiveCheck: never = guideline // TODO ts-migration
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type // TODO ts-migration
    }
  },
  newSnappingVectorIsSmallest<C extends CoordinateMarker>(l: Point<C>, r: Point<C>): boolean {
    return Math.abs(Utils.magnitude(l)) < Math.abs(Utils.magnitude(r))
  },
  newSnappingVectorIsEqual<C extends CoordinateMarker>(l: Point<C>, r: Point<C>): boolean {
    return Utils.magnitude(l) === Utils.magnitude(r)
  },
  getClosestGuidelinesAndOffsets(
    xs: Array<number>,
    ys: Array<number>,
    corners: Array<CanvasPoint>,
    guidelines: Array<Guideline>,
    constrainedDragAxis: ConstrainedDragAxis | null,
    snappingThreshold: number,
    scale: number,
  ): Array<GuidelineWithSnappingVector> {
    let xGuidelinesAndOffsets: Array<GuidelineWithSnappingVector> = []
    let yGuidelinesAndOffsets: Array<GuidelineWithSnappingVector> = []
    for (const guideline of guidelines) {
      if (guideline.type === 'XAxisGuideline' && constrainedDragAxis !== 'y') {
        const snappingVector = Guidelines.getOffsetToSnapToXGuideline(
          xs,
          guideline,
          constrainedDragAxis,
        )
        const guidelineAndOffset = {
          guideline: guideline,
          snappingVector: snappingVector,
          activateSnap: Utils.magnitude(snappingVector) < snappingThreshold / scale,
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
      } else if (guideline.type === 'YAxisGuideline' && constrainedDragAxis !== 'x') {
        const snappingVector = Guidelines.getOffsetToSnapToYGuideline(
          ys,
          guideline,
          constrainedDragAxis,
        )
        const guidelineAndOffset = {
          guideline: guideline,
          snappingVector: snappingVector,
          activateSnap: Utils.magnitude(snappingVector) < snappingThreshold / scale,
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
    Utils.fastForEach(guidelines, (guideline) => {
      if (guideline.type === 'CornerGuideline') {
        const snappingVector = Guidelines.getOffsetToSnapToCornerGuideline(corners, guideline)
        const guidelineAndOffset = {
          guideline: guideline,
          snappingVector: snappingVector,
          activateSnap: Utils.magnitude(snappingVector) < snappingThreshold / scale,
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
    })

    const guidelineResults = [...xGuidelinesAndOffsets, ...yGuidelinesAndOffsets]

    return guidelineResults.filter(
      (g) => Utils.magnitude(g.snappingVector) < (snappingThreshold / scale) * 2,
    )
  },
  offsetGuideline: function(guideline: Guideline, offset: Point<any>): Guideline {
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
      default:
        const _exhaustiveCheck: never = guideline // TODO ts-migration
        throw 'Unexpected value for guideline of type: ' + (guideline as any).type // TODO ts-migration
    }
  },
  guidelineIntersection: function(guidelines: Array<Guideline>): CanvasPoint | null {
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
