import { stripNulls } from './array-utils'
import type { Either } from './either'
import { left, right, mapEither } from './either'

enum RawModifier {}
enum WindowModifier {}
enum CanvasModifier {}
enum LocalModifier {}
type NodeGraphModifier = CanvasModifier
export type CoordinateMarker =
  | RawModifier
  | WindowModifier
  | CanvasModifier
  | LocalModifier
  | NodeGraphModifier
type PointInner = {
  x: number
  y: number
}

export type Point<C extends CoordinateMarker> = PointInner & C
export type RawPoint = RawModifier & PointInner
export type WindowPoint = WindowModifier & PointInner
export type CanvasPoint = CanvasModifier & PointInner

export type Delta = {
  x: number
  y: number
}

export interface Segment<C extends CoordinateMarker> {
  a: Point<C>
  b: Point<C>
}

export type CanvasSegment = Segment<CanvasModifier>
export function canvasSegment(a: CanvasPoint, b: CanvasPoint): CanvasSegment {
  return { a: a, b: b }
}

export function canvasPoint(p: PointInner): CanvasPoint {
  return p as CanvasPoint
}
export function localPoint(p: PointInner): LocalPoint {
  return p as LocalPoint
}
export function windowPoint(p: PointInner): WindowPoint {
  return p as WindowPoint
}

export type LocalPoint = LocalModifier & PointInner
export type NodeGraphPoint = NodeGraphModifier & PointInner
export type Vector<C extends CoordinateMarker> = Point<C>
export type WindowVector = WindowModifier & PointInner
export type CanvasVector = CanvasModifier & PointInner
export type LocalVector = LocalModifier & PointInner
export type UnsafePoint = PointInner

export function canvasVector(vector: PointInner): CanvasVector {
  return vector as CanvasVector
}

export function windowVector(vector: PointInner): WindowVector {
  return vector as WindowVector
}

export type Circle = {
  cx: number
  cy: number
  r: number
}

export type Ellipse = {
  cx: number
  cy: number
  rx: number
  ry: number
}

export interface Size {
  width: number
  height: number
}

export function size(width: number, height: number): Size {
  return {
    width: width,
    height: height,
  }
}

export function sizeFromRectangle(rectangle: Rectangle<any>): Size {
  return size(rectangle.width, rectangle.height)
}

export type SimpleRectangle = {
  x: number
  y: number
  width: number
  height: number
}

export type Rectangle<C extends CoordinateMarker> = SimpleRectangle & C
export type WindowRectangle = WindowModifier & SimpleRectangle
export type CanvasRectangle = CanvasModifier & SimpleRectangle
export type LocalRectangle = LocalModifier & SimpleRectangle
export type NodeGraphRectangle = NodeGraphModifier & SimpleRectangle

export type InfinityRectangle<C extends CoordinateMarker> = { type: 'INFINITY_RECTANGLE' } & C

export const infinityRectangle = { type: 'INFINITY_RECTANGLE' }
export const infinityCanvasRectangle = infinityRectangle as InfinityRectangle<CanvasModifier>
export const infinityLocalRectangle = infinityRectangle as InfinityRectangle<LocalModifier>

export type MaybeInfinityRectangle<C extends CoordinateMarker> = Rectangle<C> | InfinityRectangle<C>
export type MaybeInfinityCanvasRectangle = MaybeInfinityRectangle<CanvasModifier>
export type MaybeInfinityLocalRectangle = MaybeInfinityRectangle<LocalModifier>

export function isInfinityRectangle<C extends CoordinateMarker>(
  value: MaybeInfinityRectangle<C>,
): value is InfinityRectangle<C> {
  return 'type' in value && value.type === 'INFINITY_RECTANGLE'
}

export function isFiniteRectangle<C extends CoordinateMarker>(
  r: MaybeInfinityRectangle<C>,
): r is Rectangle<C> {
  return !isInfinityRectangle(r)
}

export function isNotNullFiniteRectangle<C extends CoordinateMarker>(
  r: MaybeInfinityRectangle<C> | null,
): r is Rectangle<C> {
  return r != null && isFiniteRectangle(r)
}

export function forceFiniteRectangle<C extends CoordinateMarker>(
  r: MaybeInfinityRectangle<C> | null,
): Rectangle<C> {
  if (isNotNullFiniteRectangle(r)) {
    return r
  }
  throw new Error('invariant: we expected a finite Rectangle')
}

export function canvasRectangle(rectangle: null | undefined): null
export function canvasRectangle(rectangle: SimpleRectangle): CanvasRectangle
export function canvasRectangle(
  rectangle: SimpleRectangle | null | undefined,
): CanvasRectangle | null
export function canvasRectangle(
  rectangle: SimpleRectangle | null | undefined,
): CanvasRectangle | null {
  if (rectangle == null) {
    return null
  }
  return rectangle as CanvasRectangle
}

export function localRectangle(rectangle: null | undefined): null
export function localRectangle(rectangle: SimpleRectangle): LocalRectangle
export function localRectangle(rectangle: SimpleRectangle | null | undefined): LocalRectangle | null
export function localRectangle(
  rectangle: SimpleRectangle | null | undefined,
): LocalRectangle | null {
  if (rectangle == null) {
    return null
  }
  return rectangle as LocalRectangle
}

export function windowRectangle(rectangle: null | undefined): null
export function windowRectangle(rectangle: SimpleRectangle): WindowRectangle
export function windowRectangle(
  rectangle: SimpleRectangle | null | undefined,
): WindowRectangle | null
export function windowRectangle(
  rectangle: SimpleRectangle | null | undefined,
): WindowRectangle | null {
  if (rectangle == null) {
    return null
  }
  return rectangle as WindowRectangle
}

export function zeroRectIfNullOrInfinity<C extends CoordinateMarker>(
  r: MaybeInfinityRectangle<C> | null,
): Rectangle<C> {
  return r == null || isInfinityRectangle(r) ? (zeroRectangle as Rectangle<C>) : r
}

export function nullIfInfinity<C extends CoordinateMarker>(
  r: MaybeInfinityRectangle<C> | null | undefined,
): Rectangle<C> | null {
  return r == null || isInfinityRectangle(r) ? null : r
}

export function numbersEqual(l: number, r: number): boolean {
  // in JS numbers are considered accurate up to 15 digits: https://www.w3schools.com/js/js_numbers.asp
  return l <= r + r * 1e-14 && l >= r - r * 1e-14
}

export function numberIsZero(n: number): boolean {
  return numbersEqual(n, 0)
}

export function normalizeDegrees(degrees: number): number {
  return Math.abs(degrees % 360)
}

export function degreesToRadians(degrees: number): number {
  return (degrees * Math.PI) / 180
}

export function radiansToDegrees(radians: number): number {
  return (radians * 180) / Math.PI
}

export function scalePoint<C extends CoordinateMarker>(p: Point<C>, by: Vector<C>): Point<C> {
  return {
    x: p.x * by.x,
    y: p.y * by.y,
  } as Point<C>
}

export function scaleVector<C extends CoordinateMarker>(vector: Vector<C>, by: number): Vector<C> {
  return {
    x: vector.x * by,
    y: vector.y * by,
  } as Vector<C>
}

export function rotateVector<C extends CoordinateMarker>(
  vector: Vector<C>,
  radians: number,
): Vector<C> {
  return {
    x: vector.x * Math.cos(radians) - vector.y * Math.sin(radians),
    y: vector.x * Math.sin(radians) + vector.y * Math.cos(radians),
  } as Vector<C>
}

export const zeroPoint = {
  x: 0,
  y: 0,
}

export const zeroSize = {
  width: 0,
  height: 0,
}

export const zeroRectangle = {
  x: 0,
  y: 0,
  width: 0,
  height: 0,
}
export const zeroCanvasRect = zeroRectangle as CanvasRectangle
export const zeroLocalRect = zeroRectangle as LocalRectangle
export const zeroWindowRect = zeroRectangle as WindowRectangle
export const zeroCanvasPoint = zeroPoint as CanvasPoint

export function zeroRectangleAtPoint<C extends CoordinateMarker>(p: Point<C>): Rectangle<C> {
  return {
    x: p.x,
    y: p.y,
    width: 0,
    height: 0,
  } as Rectangle<C>
}

export function rect<C extends CoordinateMarker>(
  x: number,
  y: number,
  width: number,
  height: number,
): Rectangle<C> {
  return {
    x: x,
    y: y,
    width: width,
    height: height,
  } as Rectangle<C>
}

export function point<C extends CoordinateMarker>(x: number, y: number): Point<C> {
  return {
    x: x,
    y: y,
  } as Point<C>
}

export function shiftToOrigin<C extends CoordinateMarker>(rectangle: Rectangle<C>): Rectangle<C> {
  return {
    x: 0,
    y: 0,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function rectOrigin<C extends CoordinateMarker>(rectangle: Rectangle<C>): Point<C> {
  return {
    x: rectangle.x,
    y: rectangle.y,
  } as Point<C>
}

export function sizeFitsInTarget(sizeToCheck: Size, target: Size): boolean {
  return sizeToCheck.width <= target.width && sizeToCheck.height <= target.height
}

export function rectangleContainsRectangle(
  outer: CanvasRectangle,
  inner: CanvasRectangle,
): boolean {
  return (
    outer.x < inner.x &&
    inner.x + inner.width < outer.x + outer.width &&
    outer.y < inner.y &&
    inner.y + inner.height < outer.y + outer.height
  )
}

export function rectangleFromTLBR(
  topLeft: CanvasPoint,
  bottomRight: CanvasPoint,
  preventZeroSize?: boolean,
): CanvasRectangle {
  function maybePreventZeroSize(n: number) {
    return preventZeroSize === true && n === 0 ? 1 : n
  }

  return canvasRectangle({
    x: maybePreventZeroSize(topLeft.x),
    y: maybePreventZeroSize(topLeft.y),
    width: maybePreventZeroSize(bottomRight.x - topLeft.x),
    height: maybePreventZeroSize(bottomRight.y - topLeft.y),
  })
}

export function rectContainsPoint<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  p: Point<C>,
): boolean {
  return (
    rectangle.x < p.x &&
    rectangle.y < p.y &&
    rectangle.x + rectangle.width > p.x &&
    rectangle.y + rectangle.height > p.y
  )
}

export function rectContainsPointInclusive<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  p: Point<C>,
): boolean {
  return (
    rectangle.x <= p.x &&
    rectangle.y <= p.y &&
    rectangle.x + rectangle.width >= p.x &&
    rectangle.y + rectangle.height >= p.y
  )
}

export function circleContainsPoint<C extends CoordinateMarker>(
  circle: Circle,
  p: Point<C>,
): boolean {
  const dist = magnitude({
    x: p.x - circle.cx,
    y: p.y - circle.cy,
  } as LocalPoint)
  return dist <= circle.r
}

export function ellipseContainsPoint<C extends CoordinateMarker>(
  ellipse: Ellipse,
  p: Point<C>,
): boolean {
  const { cx, cy, rx, ry } = ellipse
  const { x, y } = p
  // first check if the point is in the rectangle containing the ellipse
  if (rectContainsPoint({ x: cx, y: cy, width: rx * 2, height: ry * 2 } as Rectangle<C>, p)) {
    // now check if it is actually in the ellipse
    // https://math.stackexchange.com/questions/76457/check-if-a-point-is-within-an-ellipse/76463#76463
    const a = Math.pow(x - cx, 2) / Math.pow(rx, 2)
    const b = Math.pow(y - cy, 2) / Math.pow(ry, 2)
    return a + b <= 1
  } else {
    return false
  }
}

export function negate<C extends CoordinateMarker>(p: Point<C>): Point<C> {
  return {
    x: -p.x,
    y: -p.y,
  } as Point<C>
}

export function magnitude<C extends CoordinateMarker>(vector: Vector<C>): number {
  return Math.sqrt(Math.pow(vector.x, 2) + Math.pow(vector.y, 2))
}

export function distance<C extends CoordinateMarker>(from: Point<C>, to: Point<C>): number {
  return magnitude({ x: from.x - to.x, y: from.y - to.y } as Vector<C>)
}

export function distanceFromPointToRectangle<C extends CoordinateMarker>(
  p: Point<C>,
  rectangle: Rectangle<C>,
): number {
  // Normalize the rectangle to ensure positive width and height
  const normalizedRect = normalizeRect(rectangle)

  // Calculate the nearest point on the rectangle to the given point
  const nearestX = Math.max(
    normalizedRect.x,
    Math.min(p.x, normalizedRect.x + normalizedRect.width),
  )
  const nearestY = Math.max(
    normalizedRect.y,
    Math.min(p.y, normalizedRect.y + normalizedRect.height),
  )

  // If the nearest point is the same as the given point, it's inside or on the edge of the rectangle
  if (nearestX === p.x && nearestY === p.y) {
    return 0
  }

  // Calculate the distance between the nearest point and the given point
  return distance(p, { x: nearestX, y: nearestY } as Point<C>)
}

export function product<C extends CoordinateMarker>(a: Point<C>, b: Point<C>): number {
  return a.x * b.x + a.y * b.y
}

export function pointIsClockwiseFromLine<C extends CoordinateMarker>(
  targetPoint: Point<C>,
  linePointA: Point<C>,
  linePointB: Point<C>,
): boolean {
  // The order of line points a and b are important, as they determine which direction the line is pointing in,
  // and therefore which direction is clockwise from it
  return (
    (linePointB.x - linePointA.x) * (targetPoint.y - linePointA.y) >
    (linePointB.y - linePointA.y) * (targetPoint.x - linePointA.x)
  )
}

export function vectorFromPoints<C extends CoordinateMarker>(
  p1: Point<C>,
  p2: Point<C>,
): Vector<C> {
  return {
    x: p2.x - p1.x,
    y: p2.y - p1.y,
  } as Vector<C>
}

export function interpolateAt<C extends CoordinateMarker>(
  p1: Point<C>,
  p2: Point<C>,
  ratio: number,
): Point<C> {
  return {
    x: p1.x + (p2.x - p1.x) * ratio,
    y: p1.y + (p2.y - p1.y) * ratio,
  } as Point<C>
}

export function normalizeRect<C extends CoordinateMarker>(rectangle: Rectangle<C>): Rectangle<C> {
  const x = rectangle.width < 0 ? rectangle.x + rectangle.width : rectangle.x
  const y = rectangle.height < 0 ? rectangle.y + rectangle.height : rectangle.y
  return {
    x: x,
    y: y,
    width: Math.abs(rectangle.width),
    height: Math.abs(rectangle.height),
  } as Rectangle<C>
}

export function getLocalRectangleInNewParentContext(
  newParent: CanvasPoint,
  child: CanvasRectangle,
): LocalRectangle {
  return asLocal(offsetRect(child, negate(newParent)))
}

export function getLocalPointInNewParentContext(
  newParent: CanvasPoint,
  child: CanvasPoint,
): LocalPoint {
  return asLocal(offsetPoint(child, negate(newParent)))
}

export function getCanvasRectangleWithCanvasOffset(
  canvasOffset: CanvasPoint,
  child: LocalRectangle,
): CanvasRectangle {
  return offsetRect(asGlobal(child), canvasOffset)
}

export function getCanvasPointWithCanvasOffset(
  canvasOffset: CanvasPoint,
  child: LocalPoint,
): CanvasPoint {
  return offsetPoint(asGlobal(child), canvasOffset)
}

export function getCanvasVectorFromWindowVector(
  vector: WindowVector,
  canvasScale: number,
): CanvasVector {
  return asGlobal(scaleVector(vector, 1 / canvasScale))
}

export function asLocal(g: CanvasRectangle | WindowRectangle): LocalRectangle
export function asLocal(g: CanvasPoint | WindowPoint): LocalPoint
export function asLocal(g: CanvasVector | WindowVector): LocalVector
export function asLocal(g: any) {
  return g as any
}

export function asGlobal(l: LocalRectangle | WindowRectangle): CanvasRectangle
export function asGlobal(l: LocalPoint | WindowPoint): CanvasPoint
export function asGlobal(l: LocalVector | WindowVector): CanvasVector
export function asGlobal(g: any) {
  return g as any
}

export function rectangleDifference<C extends CoordinateMarker>(
  from: Rectangle<C>,
  to: Rectangle<C>,
): Rectangle<C> {
  return {
    x: to.x - from.x,
    y: to.y - from.y,
    width: to.width - from.width,
    height: to.height - from.height,
  } as Rectangle<C>
}

export function rectangleIntersection<C extends CoordinateMarker>(
  rect1: Rectangle<C>,
  rect2: Rectangle<C>,
): Rectangle<C> | null {
  const maxLeft = Math.max(rect1.x, rect2.x)
  const minRight = Math.min(rect1.x + rect1.width, rect2.x + rect2.width)
  const top = Math.max(rect1.y, rect2.y)
  const bottom = Math.min(rect1.y + rect1.height, rect2.y + rect2.height)
  const width = minRight - maxLeft
  const height = bottom - top
  if (width > 0 && height > 0) {
    return {
      x: maxLeft,
      y: top,
      width: minRight - maxLeft,
      height: bottom - top,
    } as Rectangle<C>
  } else {
    return null
  }
}

export function doRectanglesIntersect<C extends CoordinateMarker>(
  rect1: MaybeInfinityRectangle<C>,
  rect2: MaybeInfinityRectangle<C>,
): boolean {
  if (isInfinityRectangle(rect1) || isInfinityRectangle(rect2)) {
    // Infinity rectangles intersect with anything and everything.
    return true
  } else {
    return rectangleIntersection(rect1, rect2) != null
  }
}

export function anyRectanglesIntersect<C extends CoordinateMarker>(
  rectangles: Array<MaybeInfinityRectangle<C>>,
): boolean {
  for (let firstIndex = 0; firstIndex < rectangles.length; firstIndex++) {
    for (let secondIndex = firstIndex + 1; secondIndex < rectangles.length; secondIndex++) {
      if (doRectanglesIntersect(rectangles[firstIndex], rectangles[secondIndex])) {
        return true
      }
    }
  }
  return false
}

export function pointDifference<C extends CoordinateMarker>(
  from: Point<C>,
  to: Point<C>,
): Point<C> {
  return {
    x: to.x - from.x,
    y: to.y - from.y,
  } as Point<C>
}

export function vectorDifference<C extends CoordinateMarker>(
  from: Vector<C>,
  to: Vector<C>,
): Vector<C> {
  return pointDifference(from, to)
}

export function offsetPoint<C extends CoordinateMarker>(p: Point<C>, by: Point<C>): Point<C> {
  return {
    x: p.x + by.x,
    y: p.y + by.y,
  } as Point<C>
}

export function offsetRect<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  by: Point<C>,
): Rectangle<C> {
  return {
    x: rectangle.x + by.x,
    y: rectangle.y + by.y,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function combineRectangles<C extends CoordinateMarker>(
  first: Rectangle<C>,
  second: Rectangle<C>,
): Rectangle<C> {
  return {
    x: first.x + second.x,
    y: first.y + second.y,
    width: first.width + second.width,
    height: first.height + second.height,
  } as Rectangle<C>
}

export function boundingRectangleArray<C extends CoordinateMarker>(
  rectangles: Array<Rectangle<C> | null>,
): Rectangle<C> | null {
  const filtered = stripNulls(rectangles)
  if (filtered.length === 0) {
    return null
  }
  const [firstRectangle, ...remainingRectangles] = filtered
  if (remainingRectangles.length === 0) {
    return firstRectangle
  } else {
    return remainingRectangles.reduce(boundingRectangle, firstRectangle)
  }
}

export function boundingRectangle<C extends CoordinateMarker>(
  first: Rectangle<C>,
  second: Rectangle<C>,
): Rectangle<C> {
  const firstTL: Point<C> = first
  const firstBR = {
    x: first.x + first.width,
    y: first.y + first.height,
  } as Point<C>
  const secondTL: Point<C> = second
  const secondBR = {
    x: second.x + second.width,
    y: second.y + second.height,
  } as Point<C>
  const newTL = {
    x: Math.min(firstTL.x, secondTL.x),
    y: Math.min(firstTL.y, secondTL.y),
  } as Point<C>
  const newBR = {
    x: Math.max(firstBR.x, secondBR.x),
    y: Math.max(firstBR.y, secondBR.y),
  } as Point<C>
  return {
    x: newTL.x,
    y: newTL.y,
    width: newBR.x - newTL.x,
    height: newBR.y - newTL.y,
  } as Rectangle<C>
}

export function stretchRect<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  by: {
    w: number
    h: number
  },
): Rectangle<C> {
  const width = rectangle.width + by.w
  const height = rectangle.height + by.h
  return normalizeRect({
    x: rectangle.x,
    y: rectangle.y,
    width: width,
    height: height,
  } as Rectangle<C>)
}

export function scaleSize(sizeToScale: Size, by: number): Size {
  return {
    width: sizeToScale.width * by,
    height: sizeToScale.height * by,
  }
}

export function scaleRect<Rect extends Rectangle<C>, C extends CoordinateMarker>(
  rectangle: Rect,
  by: number,
  fromCenter: boolean = false,
): Rect {
  const width = rectangle.width * by
  const height = rectangle.height * by
  const xOffset = (width - rectangle.width) / 2
  const yOffset = (height - rectangle.height) / 2
  return {
    x: fromCenter ? rectangle.x - xOffset : rectangle.x * by,
    y: fromCenter ? rectangle.y - yOffset : rectangle.y * by,
    width: width,
    height: height,
  } as Rect
}

export function getRectCenter<C extends CoordinateMarker>(rectangle: Rectangle<C>): Point<C> {
  return {
    x: rectangle.x + rectangle.width / 2,
    y: rectangle.y + rectangle.height / 2,
  } as Point<C>
}

export function setRectLeftX<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  x: number,
): Rectangle<C> {
  return {
    x: x,
    y: rectangle.y,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectCenterX<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  x: number,
): Rectangle<C> {
  return {
    x: x - rectangle.width / 2,
    y: rectangle.y,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectRightX<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  x: number,
): Rectangle<C> {
  return {
    x: x - rectangle.width,
    y: rectangle.y,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectTopY<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  y: number,
): Rectangle<C> {
  return {
    x: rectangle.x,
    y: y,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectCenterY<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  y: number,
): Rectangle<C> {
  return {
    x: rectangle.x,
    y: y - rectangle.height / 2,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectBottomY<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  y: number,
): Rectangle<C> {
  return {
    x: rectangle.x,
    y: y - rectangle.height,
    width: rectangle.width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectWidth<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  width: number,
): Rectangle<C> {
  return {
    x: rectangle.x,
    y: rectangle.y,
    width: width,
    height: rectangle.height,
  } as Rectangle<C>
}

export function setRectHeight<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  height: number,
): Rectangle<C> {
  return {
    x: rectangle.x,
    y: rectangle.y,
    width: rectangle.width,
    height: height,
  } as Rectangle<C>
}

export function rectFromPointVector<C extends CoordinateMarker>(
  p: Point<C>,
  v: Vector<C>,
  pointIsCenter: boolean,
): Rectangle<C> {
  const vector = pointIsCenter ? { x: v.x * 2, y: v.y * 2 } : v
  const origin = pointIsCenter ? { x: p.x - v.x, y: p.y - v.y } : p
  const rectangle = {
    x: origin.x,
    y: origin.y,
    width: vector.x,
    height: vector.y,
  } as Rectangle<C>
  return normalizeRect(rectangle)
}

export function rectFromTwoPoints<C extends CoordinateMarker>(
  corner: Point<C>,
  oppositeCorner: Point<C>,
): Rectangle<C> {
  return rectFromPointVector(corner, vectorDifference(corner, oppositeCorner), false)
}

export function rectSizeToVector<C extends CoordinateMarker>(sizeOfVector: Size): Point<C> {
  return {
    x: sizeOfVector.width,
    y: sizeOfVector.height,
  } as Point<C>
}

export const roundToNearestWhole = (x: number) => roundTo(x, 0)

export function roundRectangleToNearestWhole<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
): Rectangle<C> {
  return {
    x: roundToNearestWhole(rectangle.x),
    y: roundToNearestWhole(rectangle.y),
    width: roundToNearestWhole(rectangle.width),
    height: roundToNearestWhole(rectangle.height),
  } as Rectangle<C>
}

export function transformFrameUsingBoundingBox<C extends CoordinateMarker>(
  newBoundingBox: Rectangle<C>,
  currentBoundingBox: Rectangle<C>,
  currentFrame: Rectangle<C>,
): Rectangle<C> {
  const frame = offsetRect(currentFrame, negate(currentBoundingBox as Point<C>))
  // group and multiselect resize
  const scaleWidth =
    currentBoundingBox.width === 0 ? 1 : newBoundingBox.width / currentBoundingBox.width
  const scaleHeight =
    currentBoundingBox.height === 0 ? 1 : newBoundingBox.height / currentBoundingBox.height
  const updatedFrameInBoundingBox = {
    x: frame.x * scaleWidth,
    y: frame.y * scaleHeight,
    width: currentBoundingBox.width === 0 ? newBoundingBox.width : frame.width * scaleWidth,
    height: currentBoundingBox.height === 0 ? newBoundingBox.height : frame.height * scaleHeight,
  } as Rectangle<C>
  return offsetRect(updatedFrameInBoundingBox, newBoundingBox)
}

export function closestPointOnLine<C extends CoordinateMarker>(
  lineA: Point<C>,
  lineB: Point<C>,
  p: Point<C>,
): Point<C> {
  const aToP = pointDifference(lineA, p)
  const aToB = pointDifference(lineA, lineB)
  const aToBSquared = Math.pow(aToB.x, 2) + Math.pow(aToB.y, 2)
  // the dot product of aToP and aToB
  const atpDotAtb = product(aToP, aToB)
  // The distance from A to the closest point
  const t = atpDotAtb / aToBSquared
  // Add the distance to A, moving towards B
  return {
    x: lineA.x + aToB.x * t,
    y: lineA.y + aToB.y * t,
  } as Point<C>
}

export function lineIntersection<C extends CoordinateMarker>(
  line1A: Point<C>,
  line1B: Point<C>,
  line2A: Point<C>,
  line2B: Point<C>,
): Point<C> | null {
  // from http://jsfiddle.net/Gd2S2/454/
  // if the lines intersect, the result contains the x and y of the intersection (treating the lines as infinite) and booleans for whether line segment 1 or line segment 2 contain the point
  const denominator =
    (line2B.y - line2A.y) * (line1B.x - line1A.x) - (line2B.x - line2A.x) * (line1B.y - line1A.y)
  if (denominator === 0) {
    return null
  }
  let a = line1A.y - line2A.y
  let b = line1A.x - line2A.x
  const numerator1 = (line2B.x - line2A.x) * a - (line2B.y - line2A.y) * b
  const numerator2 = (line1B.x - line1A.x) * a - (line1B.y - line1A.y) * b
  a = numerator1 / denominator
  b = numerator2 / denominator
  // if we cast these lines infinitely in both directions, they intersect here:
  // it is worth noting that this should be the same as:
  // x = line2A.x + (b * (line2B.x - line2A.x))
  // y = line2A.x + (b * (line2B.y - line2A.y))
  return {
    x: line1A.x + a * (line1B.x - line1A.x),
    y: line1A.y + a * (line1B.y - line1A.y),
  } as Point<C>
}

export function roundTo(number: number, digits: number = 0): number {
  const multiplicator = Math.pow(10, digits)
  const n = parseFloat((number * multiplicator).toFixed(11))
  return Math.round(n) / multiplicator
}

export function roundToNearestHalf(n: number): number {
  return Math.round(n * 2) / 2
}

export function roundPointToNearestWhole<C extends CoordinateMarker>(p: Point<C>): Point<C> {
  return {
    x: roundToNearestWhole(p.x),
    y: roundToNearestWhole(p.y),
  } as Point<C>
}

export function roundPointToNearestHalf<C extends CoordinateMarker>(p: Point<C>): Point<C> {
  return {
    x: roundToNearestHalf(p.x),
    y: roundToNearestHalf(p.y),
  } as Point<C>
}

export function percentToNumber(input: string): string | number {
  const newValue = Number(input.replace('%', ''))
  return isNaN(newValue) ? input : newValue / 100
}

export function numberToPercent(value: number): string {
  return roundTo(value * 100, 2) + '%'
}

export function rectangleToPoints<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
): Array<Point<C>> {
  return [
    {
      x: rectangle.x,
      y: rectangle.y,
    } as Point<C>,
    {
      x: rectangle.x + rectangle.width,
      y: rectangle.y,
    } as Point<C>,
    {
      x: rectangle.x,
      y: rectangle.y + rectangle.height,
    } as Point<C>,
    {
      x: rectangle.x + rectangle.width,
      y: rectangle.y + rectangle.height,
    } as Point<C>,
  ]
}

export function pointsEqual<C extends CoordinateMarker>(
  first: Point<C>,
  second: Point<C>,
): boolean {
  return first.x === second.x && first.y === second.y
}

export function sizesEqual(first: Size | null, second: Size | null): boolean {
  if (first == null) {
    if (second == null) {
      return true
    } else {
      return false
    }
  } else {
    if (second == null) {
      return false
    } else {
      return first.width === second.width && first.height === second.height
    }
  }
}

export function rectanglesEqual<C extends CoordinateMarker>(
  first: MaybeInfinityRectangle<C>,
  second: MaybeInfinityRectangle<C>,
): boolean {
  if (isFiniteRectangle(first) && isFiniteRectangle(second)) {
    return (
      first.x === second.x &&
      first.y === second.y &&
      first.width === second.width &&
      first.height === second.height
    )
  }
  return isInfinityRectangle(first) && isInfinityRectangle(second)
}

export function roundedRectanglesEqual<C extends CoordinateMarker>(
  first: Rectangle<C>,
  second: Rectangle<C>,
): boolean {
  return rectanglesEqual(roundRectangleToNearestWhole(first), roundRectangleToNearestWhole(second))
}

export function getRoundedRectPointsAlongAxes<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
): { horizontalPoints: Array<number>; verticalPoints: Array<number> } {
  return {
    horizontalPoints: [
      roundToNearestWhole(rectangle.x),
      roundToNearestWhole(rectangle.x + rectangle.width / 2),
      roundToNearestWhole(rectangle.x + rectangle.width),
    ],
    verticalPoints: [
      roundToNearestWhole(rectangle.y),
      roundToNearestWhole(rectangle.y + rectangle.height / 2),
      roundToNearestWhole(rectangle.y + rectangle.height),
    ],
  }
}

export function proportion(from: number, to: number): number {
  if (from < 0 || to < 0) {
    throw new Error(`Invalid parameters passed: ${from}, ${to}`)
  } else {
    if (to === 0) {
      return 1
    } else if (from === 0) {
      return 0
    } else {
      const total = from + to
      return from / total
    }
  }
}

// In radians.
// Careful as this is the angle from the _downward_ vertical.
export function angleOfPointFromVertical(p: Point<any>): number {
  // Need to flip the x coordinate because we're getting the angle
  // from the downward vertical.
  const maybeNegativeAngle = Math.atan2(-p.x, p.y)
  if (maybeNegativeAngle < 0) {
    return Math.PI * 2 + maybeNegativeAngle
  } else {
    return maybeNegativeAngle
  }
}

export function forceNotNaN(n: number, errorMessage?: string): number {
  if (isNaN(n)) {
    throw new Error(errorMessage == null ? 'Is NaN.' : errorMessage)
  } else {
    return n
  }
}

export const nanToZero = (n: number) => defaultIfNaN(n, 0)

export function defaultIfNaN(n: number, defaultValue: number): number {
  if (isNaN(n)) {
    return defaultValue
  } else {
    return n
  }
}

export function safeParseInt(raw: string): number | null {
  const result = Number.parseInt(raw)
  return isNaN(result) ? null : result
}

export function clampValue(value: number, minimum: number, maximum: number): number {
  return Math.max(Math.min(value, maximum), minimum)
}

export function wrapValue(value: number, minimum: number, maximum: number): number {
  const range = maximum - minimum + 1 // (+ 1) to include boundaries
  if (range === 0) {
    return minimum
  }
  return minimum + ((((value - minimum) % range) + range) % range)
}

export function parseNumber(value: string): Either<string, number> {
  const n = parseFloat(value)
  return isNaN(n) ? left(`${value} is not a number`) : right(n)
}

export interface NumberOrPercent {
  value: number
  isPercent: boolean
}

export function numberOrPercent(value: number, isPercent: boolean): NumberOrPercent {
  return {
    value: value,
    isPercent: isPercent,
  }
}

export function parseNumberOrPercent(value: unknown): Either<string, NumberOrPercent> {
  switch (typeof value) {
    case 'number':
      return right(numberOrPercent(value, false))
    case 'string':
      const split = value.split('%')
      const isPercent = split.length !== 1
      const parsedNumber = parseNumber(split[0])
      return mapEither((r) => {
        return { value: r, isPercent: isPercent }
      }, parsedNumber)
    default:
      return left(`Cannot parse as number or percent: ${value}`)
  }
}

export function printNumberOrPercent(number: NumberOrPercent): number | string {
  if (number.isPercent) {
    return `${number.value}%`
  } else {
    return number.value
  }
}

export function numberOrPercentToNumber(n: NumberOrPercent, outOf: number): number {
  return n.isPercent ? (n.value * outOf) / 100 : n.value
}

export function clamp(min: number, max: number, value: number): number {
  if (value < min) {
    return min
  } else if (value > max) {
    return max
  } else {
    return value
  }
}

export function canvasRectangleToLocalRectangle(
  canvasRect: CanvasRectangle,
  parentRect: CanvasRectangle,
): LocalRectangle {
  const diff = roundPointToNearestHalf(pointDifference(parentRect, canvasRect))
  return localRectangle({
    x: diff.x,
    y: diff.y,
    width: canvasRect.width,
    height: canvasRect.height,
  })
}

// https://algs4.cs.princeton.edu/91primitives/
function segmentsIntersect(a: CanvasSegment, b: CanvasSegment): boolean {
  function counterClockwise(p1: CanvasPoint, p2: CanvasPoint, p3: CanvasPoint): number {
    return (p2.y - p1.y) * (p3.x - p1.x) - (p3.y - p1.y) * (p2.x - p1.x)
  }

  if (counterClockwise(a.a, a.b, b.a) * counterClockwise(a.a, a.b, b.b) > 0) {
    return false
  }
  if (counterClockwise(b.a, b.b, a.a) * counterClockwise(b.a, b.b, a.b) > 0) {
    return false
  }

  return true
}

export function segmentIntersection(
  leftSegment: CanvasSegment,
  rightSegment: CanvasSegment,
): CanvasPoint | null {
  const pointOfIntersection = lineIntersection(
    leftSegment.a,
    leftSegment.b,
    rightSegment.a,
    rightSegment.b,
  )
  if (segmentsIntersect(leftSegment, rightSegment)) {
    return pointOfIntersection
  }
  return null
}

/**
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Remainder#description
 * This is the modulo function, be careful as % is the remainder operator.
 * The main difference is when using negative n, for example "-1 % 4 = -1" but "mod(-1, 4) = 3"
 * For two values of the same sign, the two are equivalent, but when the operands are of different signs,
 * the modulo result always has the same sign as the divisor, while the remainder has the same sign as the dividend,
 * which can make them differ by one unit of d.
 */
export function mod(n: number, m: number): number {
  return ((n % m) + m) % m
}

interface ResizeOptions {
  desiredWidth: number
  desiredHeight: number
  keepAspectRatio: boolean
  centerPoint: CanvasPoint
}

export function resizeCanvasRectangle(
  rectangle: CanvasRectangle,
  options: ResizeOptions,
): CanvasRectangle {
  const resizeI = (dimensions: { width: number; height: number }): CanvasRectangle => {
    const { width, height } = dimensions
    return canvasRectangle({
      x: options.centerPoint.x - width / 2,
      y: options.centerPoint.y - height / 2,
      width: width,
      height: height,
    })
  }

  if (options.keepAspectRatio) {
    const aspectRatio = rectangle.width / rectangle.height
    options.desiredHeight = (options.desiredWidth / aspectRatio) ^ 0
    return resizeI({ width: options.desiredWidth, height: options.desiredHeight })
  }

  return resizeI({ width: options.desiredWidth, height: options.desiredHeight })
}

export function resize(
  originalSize: Size,
  desiredSize: Size,
  mode: 'force' | 'keep-aspect-ratio',
): Size {
  if (mode === 'force') {
    return desiredSize
  }

  const aspectRatio = originalSize.width / originalSize.height
  const desiredHeight = (desiredSize.width / aspectRatio) ^ 0
  return size(desiredSize.width, desiredHeight)
}
