import * as Chroma from 'chroma-js'
import Utils, { HSLA } from '../utils/utils'
import { Color } from '../utils/utils'
import { Point } from '../core/shared/math-utils'

import * as linearEase from 'eases/linear'
import * as quadInEase from 'eases/quad-in'
import * as quadOutEase from 'eases/quad-out'
import * as quadInOutEase from 'eases/quad-in-out'
import * as TPP from './template-property-path'

export const toChroma = Utils.withDoc(_toChroma, '(color: Color): chroma.Color', '')
function _toChroma(color: Color): chroma.Color {
  return Chroma([color.red, color.green, color.blue, color.alpha])
}

export const fromChroma = Utils.withDoc(_fromChroma, '(chroma: chroma.Color): Color', '')
function _fromChroma(chroma: chroma.Color): Color {
  const [red, green, blue, alpha] = chroma.rgba()
  return {
    red: red,
    green: green,
    blue: blue,
    alpha: alpha,
  }
}

export const withChroma = Utils.withDoc(_withChroma, '(color: Color, value: number): Color', '')
function _withChroma(color: Color, transform: (chroma: any) => any): Color {
  const chromaColor = _toChroma(color)
  const chromaResult = transform(chromaColor)
  return _fromChroma(chromaResult)
}

export const gradient = Utils.withDoc(
  _gradient,
  '(angle: number, source: any, rotationH: number, ratioS: number, ratioL: number): object',
  'returns a rotated and (de)saturation and (de)luminous gradient object, usable as a background image',
)

function _gradient(
  angle: number,
  source: any,
  rotationH: number,
  ratioS: number,
  ratioL: number,
): object {
  /* based on https://github.com/jxnblk/shade/blob/gh-pages/components/Gradient.jsx */
  /* TODO support HSLA types and replace all of this mess with https://github.com/Qix-/color/blob/master/index.js */
  const baseColor = { h: source.h, s: source.s, l: source.l, a: source.a }

  // we saturate and lighten the base
  const fromColor = {
    h: hueRotate(baseColor.h, rotationH),
    s: saturate(baseColor.s, ratioS),
    l: lighten(baseColor.l, ratioL),
    a: baseColor.a,
  }

  const toColor = {
    h: hueRotate(baseColor.h, 1 - rotationH),
    s: desaturate(baseColor.s, ratioS),
    l: darken(baseColor.l, ratioL),
    a: baseColor.a,
  }

  const fromColorString = `hsla(${fromColor.h}, ${fromColor.s}%, ${fromColor.l}%, ${fromColor.a})`
  const toColorString = `hsla(${toColor.h}, ${toColor.s}%, ${toColor.l}%, ${toColor.a})`

  return {
    backgroundImage: `linear-gradient(${angle}deg, ${fromColorString}, ${toColorString} )`,
  }
}

export function hueRotate(hue: number, rotation: number): number {
  const rawTargetHue = (hue + rotation) % 360
  let targetHue = 0
  if (rawTargetHue < 0) {
    targetHue = Math.round(360 + rawTargetHue)
  } else {
    targetHue = Math.round(rawTargetHue)
  }
  return targetHue
}

/* these are all non-standard as they take a SINGLE VALUE, not a color */
/* using a standard color lib takes care of that */
function saturate(saturation: number, ratio: number): number {
  return Math.round(saturation + saturation * ratio)
}

function desaturate(saturation: number, ratio: number): number {
  return Math.round(saturation - saturation * ratio)
}

function lighten(lightness: number, ratio: number): number {
  return Math.round(lightness + lightness * ratio)
}

function darken(lightness: number, ratio: number): number {
  return Math.round(lightness - lightness * ratio)
}

export const hsla = Utils.withDoc(
  _hsla,
  '(hue: number, saturation: number, luminosity: number, alpha: number): HSLA',
  'Returns an HSLA color object',
)

function _hsla(hue: number, saturation: number, luminosity: number, alpha: number): HSLA {
  return { h: hue, s: saturation, l: luminosity, a: alpha }
}

export const abs = Utils.withDoc(
  _abs,
  '(x: number): number',
  'Returns the absolute value of a number',
)
function _abs(x: number): number {
  return Math.abs(x)
}

export const round = Utils.withDoc(
  _round,
  '(x: number): number',
  'Returns the nearest single precision float representation of a number',
)
function _round(x: number): number {
  return Math.round(x)
}

export const ceil = Utils.withDoc(
  _ceil,
  '(x: number): number',
  'Returns the smallest integer greater than or equal to a number',
)
function _ceil(x: number): number {
  return Math.ceil(x)
}

export const PI = Utils.withDoc(
  _PI,
  '(): number',
  'Ratio of the circumference of a circle to its diameter, approximately 3.14159',
)
function _PI(): number {
  return Math.PI
}

export const sin = Utils.withDoc(_sin, '(x: number): number', 'Returns the sine of a number')
function _sin(x: number): number {
  return Math.sin(x)
}

export const sinh = Utils.withDoc(
  _sinh,
  '(x: number): number',
  'Returns the hyperbolic sine of a number',
)
function _sinh(x: number): number {
  return Math.sinh(x)
}

export const asin = Utils.withDoc(_asin, '(x: number): number', 'Returns the arcsine of a number')
function _asin(x: number): number {
  return Math.asin(x)
}

export const asinh = Utils.withDoc(
  _asinh,
  '(x: number): number',
  'Returns the hyperbolic arcsine of a number',
)
function _asinh(x: number): number {
  return Math.asinh(x)
}

export const cos = Utils.withDoc(_cos, '(x: number): number', 'Returns the cosine of a number')
function _cos(x: number): number {
  return Math.cos(x)
}

export const cosh = Utils.withDoc(
  _cosh,
  '(x: number): number',
  'Returns the hyperbolic cosine of a number',
)
function _cosh(x: number): number {
  return Math.cosh(x)
}

export const acos = Utils.withDoc(_acos, '(x: number): number', 'Returns the arccosine of a number')
function _acos(x: number): number {
  return Math.acos(x)
}

export const acosh = Utils.withDoc(
  _acosh,
  '(x: number): number',
  'Returns the hyperbolic arccosine of a number',
)
function _acosh(x: number): number {
  return Math.acosh(x)
}

export const tan = Utils.withDoc(_tan, '(x: number): number', 'Returns the tangent of a number')
function _tan(x: number): number {
  return Math.tan(x)
}

export const atan = Utils.withDoc(
  _atan,
  '(x: number): number',
  'Returns the arctangent of a number',
)
function _atan(x: number): number {
  return Math.atan(x)
}

export const atan2 = Utils.withDoc(
  _atan2,
  '(y: number, x: number): number',
  'Returns the arctangent of the quotient of its arguments',
)
function _atan2(y: number, x: number): number {
  return Math.atan2(y, x)
}

export const sign = Utils.withDoc(
  _sign,
  '(x: number): number',
  'Returns the sign of the x, indicating whether x is positive, negative or zero',
)
function _sign(x: number): number {
  return Math.sign(x)
}

export const tanh = Utils.withDoc(
  _tanh,
  '(x: number): number',
  'Returns the hyperbolic tangent of a number',
)
function _tanh(x: number): number {
  return Math.tanh(x)
}

/* custom math functions */
export const random = Utils.withDoc(
  _random,
  '(low: number, high: number, int?: boolean): number',
  'Returns a random number inside the range',
)
function _random(low: number, high: number, int?: boolean): number {
  /* gets a not-really-random number (or optionally integer) within a range */
  const x = Math.sin(1) * 10000
  const rand = x - Math.floor(x)

  if (int) {
    return Math.floor(rand * (Math.ceil(high) - Math.floor(low) + 1)) + Math.floor(low)
  } else {
    return rand * (high - low) + low
  }
}

export const nearestIncrement = Utils.withDoc(
  _nearestIncrement,
  'value: number, increment?: number): number',
  'Returns the number rounded to the nearest increment. You can use eg 5 to get five-pixel intervals, or .5 to force nearest half-pixel positioning',
)
function _nearestIncrement(value: number, increment?: number): number {
  /* use for mapping eg to half-pixels, or intervals */
  if (increment) {
    return Math.round((value * 1) / increment) / (1 / increment)
  }
  return value
}

export const limit = Utils.withDoc(
  _limit,
  'value: number, lower: number, upper: number): number',
  'If the number is outside of the lower and upper limit, cap it to that limit.',
)
function _limit(value: number, lower: number, upper: number): number {
  /* limits a number to within a certain range */
  return Math.min(Math.max(lower, value), upper)
}

export const lerp = Utils.withDoc(
  _lerp,
  'value: number, fromLow: number, fromHigh: number, toLow: number, toHigh: number): number',
  'Interpolates & extrapolates a number. E.g. lerp(number, 0, view.width, 0, 1) gets the percentage (0,1) of an x position inside the view.',
)
function _lerp(
  value: number,
  fromLow: number,
  fromHigh: number,
  toLow: number,
  toHigh: number,
): number {
  /* interpolates & extrapolates. */
  return toLow + ((value - fromLow) / (fromHigh - fromLow)) * (toHigh - toLow)
}

export const modulate = Utils.withDoc(
  _modulate,
  'value: number, fromLow: number, fromHigh: number, toLow: number, toHigh: number, limit?: boolean): number',
  'Similar to LERP, but has optional limiting and works better with inverted ranges',
)
function _modulate(
  value: number,
  fromLow: number,
  fromHigh: number,
  toLow: number,
  toHigh: number,
  shouldLimit?: boolean,
): number {
  /* similar to LERP, but has optional limiting and works better with inverted ranges */
  const projection: number = toLow + ((value - fromLow) / (fromHigh - fromLow)) * (toHigh - toLow)

  if (shouldLimit) {
    if (toLow < toHigh) {
      if (projection < toLow) {
        return toLow
      }
      if (projection > toHigh) {
        return toHigh
      }
    } else {
      if (projection > toLow) {
        return toLow
      }
      if (projection < toHigh) {
        return toHigh
      }
    }
  }
  return projection
}

export const bounce = Utils.withDoc(_bounce, 't: number, p: number): number', 'description')
function _bounce(t: number, p: number): number {
  /* 0 < p < 1, bounciness) */
  return Math.pow(2, -10 * t) * Math.sin(((t - p / 4) * (2 * Math.PI)) / p) + 1
}

export const unit = Utils.withDoc(
  _unit,
  'value: number, fromLow: number, fromHigh: number): number',
  'Returns a relative position from 0 to 1. E.g. unit(150, 100, 200) returns 0.5.',
)
function _unit(value: number, fromLow: number, fromHigh: number): number {
  /* returns value from 0 to 1, clamped, of a number in a range) */
  const result = (value - fromLow) / (fromHigh - fromLow)
  return Math.max(0, Math.min(result, 1))
}

// vector helpers
export const radiansToDegrees = Utils.withDoc(
  _radiansToDegrees,
  'radians: number): number',
  'converts radians (from most trig functions like Sin, Cos etc) to degrees (that you can use in the editor)',
)
function _radiansToDegrees(radians: number): number {
  return Utils.radiansToDegrees(radians)
}

export const degreesToRadians = Utils.withDoc(
  _degreesToRadians,
  'degrees: number): number',
  'converts degrees (0 to 360) to Radians (for trigonometry functiosns like sin, cos etc',
)
function _degreesToRadians(degrees: number): number {
  return Utils.degreesToRadians(degrees)
}

export const distance = Utils.withDoc(
  _distance,
  'from: Point, to: Point): number',
  'Gets the distance between two Points. (A Point is an vector with {x: number, y: number})',
)
function _distance(from: Point<any>, to: Point<any>): number {
  return Utils.distance(from, to)
}

export const normalize = Utils.withDoc(
  _normalize,
  'vector: Point): Point',
  'Returns a vector with a length of 1. ',
)
function _normalize(vector: Point<any>): Point<any> {
  return Utils.scalePoint(vector, {
    x: 1 / Utils.magnitude(vector),
    y: 1 / Utils.magnitude(vector),
  })
}

export const negate = Utils.withDoc(
  _negate,
  'point: Point): Point',
  'A vector like the input, but in the opposite direction.',
)
function _negate(point: Point<any>): Point<any> {
  return Utils.negate(point)
}

export const subtract = Utils.withDoc(
  _subtract,
  'from: Point, to: Point): Point',
  'Subtract the first vector from the second. Pay attention to the order!',
)
function _subtract(from: Point<any>, to: Point<any>): Point<any> {
  return Utils.pointDifference(from, to)
}

export const multiply = Utils.withDoc(
  _multiply,
  'point: Point, by: number): Point',
  'Multiplies a vector with a number. Useful for scaling, lengthening etc. Also check out normalize().',
)
function _multiply(point: Point<any>, by: number): Point<any> {
  return Utils.scalePoint(point, { x: by, y: by })
}

export type Easing = (value: number) => number

export type MultiEaseRange = {
  from: number
  to: number
  easeFrom: number
  easeTo: number
  easing: Easing
}

export const linear = linearEase
export const quadIn = quadInEase
export const quadOut = quadOutEase
export const quadInOut = quadInOutEase

export const multiEaseRange = Utils.withDoc(
  _multiEaseRange,
  "from: number, to: number, easeFrom: number, easeTo: number, easing: 'linear' | 'quadIn' | 'quadOut' | 'quadInOut'): MultiEaseRange",
  'Use this inside multiEase to create ranges. Easing takes a number of functions, default is quadInOutEase()',
)
function _multiEaseRange(
  from: number,
  to: number,
  easeFrom: number,
  easeTo: number,
  easing: Easing = quadInOutEase,
): MultiEaseRange {
  return {
    from: from,
    to: to,
    easeFrom: easeFrom,
    easeTo: easeTo,
    easing: easing,
  }
}

export const multiEase = Utils.withDoc(
  _multiEase,
  'value: number, ...ranges: Array<MultiEaseRange>): number',
  'Creates the equivalent of an ASDR envelope for smooth transitions along a range. Use with multiEaseRange to easily create ranges.',
)
function _multiEase(value: number, ...ranges: Array<MultiEaseRange>): number {
  const rangeToUse = ranges.find((range) => {
    const lowerEnd = Math.min(range.from, range.to)
    const higherEnd = Math.max(range.from, range.to)
    return value >= lowerEnd && value <= higherEnd
  })
  if (rangeToUse == null) {
    return 0
  } else {
    const fullRangeDifference = rangeToUse.to - rangeToUse.from
    const valueDifference = value - rangeToUse.from
    const ratio = limit(valueDifference / fullRangeDifference, 0, 1)
    const easedRatio = rangeToUse.easing(ratio)
    const easeFullRangeDifference = rangeToUse.easeTo - rangeToUse.easeFrom
    const result = rangeToUse.easeFrom + easedRatio * easeFullRangeDifference
    return result
  }
}

export const tppEqual = Utils.withDoc(
  TPP.pathsEqual,
  '(first: TemplatePropertyPath, second: TemplatePropertyPath): boolean',
  'Returns true if the two paths refer to the same property.',
)
