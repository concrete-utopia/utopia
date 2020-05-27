import { stripNulls } from './array-utils'
import { Either, left, right, mapEither } from './either'

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
export function canvasPoint(p: PointInner): CanvasPoint {
  return p as CanvasPoint
}

export type LocalPoint = LocalModifier & PointInner
export type NodeGraphPoint = NodeGraphModifier & PointInner
export type Vector<C extends CoordinateMarker> = Point<C>
export type WindowVector = WindowModifier & PointInner
export type CanvasVector = CanvasModifier & PointInner
export type LocalVector = LocalModifier & PointInner
export type UnsafePoint = PointInner

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

export type Size = {
  width: number
  height: number
}

export type RectangleInner = {
  x: number
  y: number
  width: number
  height: number
}

export type SimpleRectangle = RectangleInner
export type Rectangle<C extends CoordinateMarker> = RectangleInner & C
export type WindowRectangle = WindowModifier & RectangleInner
export type CanvasRectangle = CanvasModifier & RectangleInner
export type LocalRectangle = LocalModifier & RectangleInner
export type NodeGraphRectangle = NodeGraphModifier & RectangleInner

export function canvasRectangle(rectangle: null | undefined): null
export function canvasRectangle(rectangle: RectangleInner): CanvasRectangle
export function canvasRectangle(
  rectangle: RectangleInner | null | undefined,
): CanvasRectangle | null
export function canvasRectangle(
  rectangle: RectangleInner | null | undefined,
): CanvasRectangle | null {
  if (rectangle == null) {
    return null
  }
  return rectangle as CanvasRectangle
}

export function localRectangle(rectangle: null | undefined): null
export function localRectangle(rectangle: RectangleInner): LocalRectangle
export function localRectangle(rectangle: RectangleInner | null | undefined): LocalRectangle | null
export function localRectangle(
  rectangle: RectangleInner | null | undefined,
): LocalRectangle | null {
  if (rectangle == null) {
    return null
  }
  return rectangle as LocalRectangle
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

export function rectSize(rectangle: Rectangle<any>): Size {
  return {
    width: rectangle.width,
    height: rectangle.height,
  }
}

export function setRectSize<C extends CoordinateMarker>(
  rectangle: Rectangle<C>,
  size: Size,
): Rectangle<C> {
  return {
    x: rectangle.x,
    y: rectangle.y,
    width: size.width,
    height: size.height,
  } as Rectangle<C>
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

export function product<C extends CoordinateMarker>(a: Point<C>, b: Point<C>): number {
  return a.x * b.x + a.y * b.y
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

export function scaleSize(size: Size, by: number): Size {
  return {
    width: size.width * by,
    height: size.height * by,
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

export function rectSizeToVector<C extends CoordinateMarker>(size: Size): Point<C> {
  return {
    x: size.width,
    y: size.height,
  } as Point<C>
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

export function roundPointTo<C extends CoordinateMarker>(p: Point<C>, precision: number): Point<C> {
  return {
    x: roundTo(p.x, precision),
    y: roundTo(p.y, precision),
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

export function rectanglesEqual(first: RectangleInner, second: RectangleInner): boolean {
  return (
    first.x === second.x &&
    first.y === second.y &&
    first.width === second.width &&
    first.height === second.height
  )
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

export function nanToZero(n: number): number {
  if (isNaN(n)) {
    return 0
  } else {
    return n
  }
}

export function safeParseInt(s: string): number {
  const n = Number.parseInt(s)
  return forceNotNaN(n, `Unable to parse ${s}.`)
}

export function clampValue(value: number, minimum: number, maximum: number): number {
  return Math.max(Math.min(value, maximum), minimum)
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
