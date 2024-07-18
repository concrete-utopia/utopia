// FIXME This file shouldn't live under the inspector, and shouldn't be defining types
import Chroma from 'chroma-js'
import fastDeepEqual from 'fast-deep-equal'
import type { Property } from 'csstype'
import type { FramePin } from 'utopia-api/core'
import {
  FlexAlignment,
  FlexJustifyContent,
  FlexWrap,
  LayoutSystem,
  UtopiaUtils,
} from 'utopia-api/core'
import type { LayoutPropertyTypes, StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import { findLastIndex } from '../../../core/shared/array-utils'
import type { Either, Right as EitherRight } from '../../../core/shared/either'
import {
  applicative2Either,
  bimapEither,
  eitherToMaybe,
  flatMapEither,
  isLeft,
  isRight,
  left,
  leftMapEither,
  mapEither,
  right,
  traverseEither,
} from '../../../core/shared/either'
import type {
  JSExpression,
  JSXAttributes,
  JSExpressionValue,
  JSXElement,
  GridPosition,
  GridRange,
  GridAutoOrTemplateBase,
  GridContainerProperties,
} from '../../../core/shared/element-template'
import {
  emptyComments,
  modifiableAttributeIsAttributeFunctionCall,
  modifiableAttributeIsAttributeNotFound,
  modifiableAttributeIsPartOfAttributeValue,
  isRegularJSXAttribute,
  jsExpressionFunctionCall,
  jsExpressionValue,
  gridPositionValue,
  gridRange,
  gridAutoOrTemplateDimensions,
} from '../../../core/shared/element-template'
import type { ModifiableAttribute } from '../../../core/shared/jsx-attributes'
import {
  getJSExpressionAtPath,
  jsxFunctionAttributeToRawValue,
} from '../../../core/shared/jsx-attributes'
import {
  getJSXAttributesAtPath,
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
  setJSXValueInAttributeAtPath,
} from '../../../core/shared/jsx-attribute-utils'
import type { NumberOrPercent } from '../../../core/shared/math-utils'
import {
  numberOrPercentToNumber,
  parseNumber,
  parseNumberOrPercent,
} from '../../../core/shared/math-utils'
import type { PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { assertNever, type PrimitiveType, type ValueOf } from '../../../core/shared/utils'
import { parseBackgroundSize } from '../../../printer-parsers/css/css-parser-background-size'
import { parseBorder } from '../../../printer-parsers/css/css-parser-border'
import Utils from '../../../utils/utils'
import { fontFamilyArrayToCSSFontFamilyString } from '../sections/style-section/text-subsection/fonts-list'
import {
  parsePadding,
  printPaddingAsAttributeValue,
} from '../../../printer-parsers/css/css-parser-padding'
import {
  parseMargin,
  printMarginAsAttributeValue,
} from '../../../printer-parsers/css/css-parser-margin'
import { parseFlex, printFlexAsAttributeValue } from '../../../printer-parsers/css/css-parser-flex'
import { memoize } from '../../../core/shared/memoize'
import { parseCSSArray } from '../../../printer-parsers/css/css-parser-utils'
import type { ParseError } from '../../../utils/value-parser-utils'
import { descriptionParseError } from '../../../utils/value-parser-utils'
import * as csstree from 'css-tree'
import { expandCssTreeNodeValue, parseCssTreeNodeValue } from './css-tree-utils'

var combineRegExp = function (regexpList: Array<RegExp | string>, flags?: string) {
  let source: string = ''
  for (let i = 0; i < regexpList.length; i++) {
    const item = regexpList[i]
    source += typeof item === 'string' ? item : item.source
  }
  return new RegExp(source, flags != null ? flags : '')
}

export type CSSTransformSingleSupportedType =
  | 'rotate'
  | 'rotateX'
  | 'rotateY'
  | 'rotateZ'
  | 'scaleX'
  | 'scaleY'
  | 'scaleZ'
  | 'skewX'
  | 'skewY'
  | 'translateX'
  | 'translateY'
  | 'translateZ'

export type CSSTransformDoubleSupportedType = 'scale' | 'skew' | 'translate'

export type CSSTransformSupportedType =
  | CSSTransformSingleSupportedType
  | CSSTransformDoubleSupportedType

export const cssTransformSupportedValidFunctionNames: Array<CSSTransformSupportedType> = [
  'rotate',
  'rotateX',
  'rotateY',
  'rotateZ',
  'scale',
  'scaleX',
  'scaleY',
  'scaleZ',
  'skew',
  'skewX',
  'skewY',
  'translate',
  'translateX',
  'translateY',
  'translateZ',
]
export const cssTransformUnsupportedValidFunctionNames = [
  'matrix',
  'matrix3d',
  'translate3d',
  'scale3d',
  'rotate3d',
  'perspective',
]

export const RegExpLibrary = (function GenerateRegExpLib() {
  const comma = /\s*,\s*/ // Allow space around comma.
  const optionalSpace = '\\s*'
  const space = '\\s+'
  const openComment = /(\/\*)?/
  const closeComment = /(\*\/)?/

  const searchFlags = 'gi' // ignore case for angles, "rgb" etc
  const angle = /(?:[+-]?\d*\.?\d+)(?:deg|grad|rad|turn)/ // Angle +ive, -ive and angle types
  const colorHex = /\#(?:[A-Fa-f0-9]{8}|[A-Fa-f0-9]{6}|[A-Fa-f0-9]{4}|[A-Fa-f0-9]{3})/ // #RGB #RGBA #RRGGBB #RRGGBBAA
  const colorHexOptionalOctothorp =
    /(\#?)([A-Fa-f0-9]{8}|[A-Fa-f0-9]{6}|[A-Fa-f0-9]{4}|[A-Fa-f0-9]{3})/ // #RGB #RGBA #RRGGBB #RRGGBBAA RGB RGBA RRGGBB RRGGBBAA
  const colorHSLaComponents =
    /hsla?(\((\d*\.?\d+)(deg|grad|rad|turn)?\s*,\s*([\d.]+)%\s*,\s*([\d.]+)%(\s*,\s*(\d*\.?\d+%?))?\)|\((\d*\.?\d+)(deg|grad|rad|turn)?\s*([\d.]+)%\s*([\d.]+)%(\s*\/\s*(\d*\.?\d+%?))?\))/
  const colorHSLa =
    /hsla?(?:\((?:\d*\.?\d+)(?:deg|grad|rad|turn)?\s*,\s*(?:[\d.]+)%\s*,\s*(?:[\d.]+)%(?:\s*,\s*(?:\d*\.?\d+%?))?\)|\((?:\d*\.?\d+)(?:deg|grad|rad|turn)?\s*(?:[\d.]+)%\s*(?:[\d.]+)%(?:\s*\/\s*(?:\d*\.?\d+%?))?\))/
  const colorRGBaComponents =
    /rgba?(?:\((\d*\.?\d+)\s*,\s*(\d*\.?\d+)\s*,\s*(\d*\.?\d+)\s*(?:,\s*(\d*\.?\d+%?))?\)|\((\d*\.?\d+)\s*(\d*\.?\d+)\s*(\d*\.?\d+)\s*(?:\/\s*(\d*\.?\d+%?))?\)|\((\d*\.?\d+%)\s*,\s*(\d*\.?\d+%)\s*,\s*(\d*\.?\d+%)(?:\s*,\s*(\d*\.?\d+%?))?\))/
  const colorRGBa =
    /rgba?(?:\((?:\d*\.?\d+)\s*,\s*(?:\d*\.?\d+)\s*,\s*(?:\d*\.?\d+)\s*(?:,\s*(?:\d*\.?\d+%?))?\)|\((?:\d*\.?\d+)\s*(?:\d*\.?\d+)\s*(?:\d*\.?\d+)\s*(?:\/\s*(?:\d*\.?\d+%?))?\)|\((?:\d*\.?\d+%)\s*,\s*(?:\d*\.?\d+%)\s*,\s*(?:\d*\.?\d+%)(?:\s*,\s*(?:\d*\.?\d+%?))?\))/
  const colorKeyword =
    /(?:aliceblue|antiquewhite|aqua|aquamarine|azure|beige|bisque|black|blanchedalmond|blue|blueviolet|brown|burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|cyan|darkblue|darkcyan|darkgoldenrod|darkgray|darkgreen|darkgrey|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|fuchsia|gainsboro|ghostwhite|gold|goldenrod|gray|green|greenyellow|grey|honeydew|hotpink|indianred|indigo|ivory|khaki|lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|lightgray|lightgreen|lightgrey|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|lightslategrey|lightsteelblue|lightyellow|lime|limegreen|linen|magenta|maroon|mediumaquamarine|mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|navy|oldlace|olive|olivedrab|orange|orangered|orchid|palegoldenrod|palegreen|paleturquoise|palevioletred|papayawhip|peachpuff|peru|pink|plum|powderblue|purple|rebeccapurple|red|rosybrown|royalblue|saddlebrown|salmon|sandybrown|seagreen|seashell|sienna|silver|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|tan|teal|thistle|tomato|turquoise|violet|wheat|white|whitesmoke|yellow|yellowgreen)/
  // prettier-ignore
  const color = combineRegExp([
    '(',
    colorHex,
    '|',
    colorHSLa,
    '|',
    colorRGBa,
    '|',
    colorKeyword,
    ')',
  ])
  const colorComponents = combineRegExp([
    '(?:',
    colorHex,
    '|',
    colorHSLaComponents,
    '|',
    colorRGBaComponents,
    '|',
    colorKeyword,
    ')',
  ])
  const cssPixelValue = /(?!0\d)(?:(?:[+-]?\d*\.?\d+)(?=px)|0(?!=px))(?:px)?/ // '0', '100px', '.05px'
  const percentageValue = /(?:[+-]?\d*\.?\d+)(?:%)/ // '100%', '.05%'
  // prettier-ignore
  const gradientColorStop = combineRegExp([
    colorComponents,
    space,
    '(?:',
    percentageValue,
    ')'
  ])
  // prettier-ignore
  const gradientColorStops = combineRegExp([
    '(?:',
    gradientColorStop,
    comma,
    ')*',
    gradientColorStop
  ])
  // Capture 0: angle, 1: stop list
  // prettier-ignore
  const gradientPosition = combineRegExp([
    '(?:',
    optionalSpace,
    'at',
    space,
    '(',
    percentageValue,
    ')',
    space,
    '(',
    percentageValue,
    ')',
    optionalSpace,
    ')?',
  ])

  // Capture 1: angle, 2: all stops
  // prettier-ignore
  const linearGradient = combineRegExp([
    'linear-gradient\\(',
    optionalSpace,
    '(?:',
    '(',
    angle,
    ')',
    comma,
    ')?',
    '(',
    gradientColorStops,
    ')',
    optionalSpace,
    '\\)'
  ],
    searchFlags,
  )

  // prettier-ignore
  const radialGradientSize = combineRegExp([
    '(?:',
    '(?:',
    '(',
    percentageValue,
    ')',
    space,
    '(',
    percentageValue,
    ')',
    ')',
    ')?',
    optionalSpace,
  ])
  // Capture 0: circle, 1: circle size, 2: circle extent, 3: ellipse, 4: ellipse width, 5: ellipse height, 6: ellipse extent, 7: position x, 8: position y, all stops
  // prettier-ignore
  const radialGradient = combineRegExp(
    [
      'radial-gradient\\(',
      '(?:',
      radialGradientSize,
      gradientPosition,
      comma,
      ')?',
      '(',
      gradientColorStops,
      ')',
      '\\)'
    ],
    searchFlags,
  )
  // Capture 1: angle, 2: position x, 3: position y, 4: stop list
  // prettier-ignore
  const conicGradient = combineRegExp([
    'conic-gradient\\(',
    '(?:',
    optionalSpace,
    '(?:',
    'from',
    space,
    '(',
    angle,
    '|',
    percentageValue,
    ')',
    ')?',
    gradientPosition,
    comma,
    ')?',
    '(',
    gradientColorStops,
    ')',
    optionalSpace,
    '\\)',
  ],
    searchFlags,
  )

  // Capture 1: color and 27: position
  // prettier-ignore
  const gradientColorStopValues = combineRegExp([
    optionalSpace,
    '(',
    colorComponents,
    ')',
    '(?:',
    space,
    '(',
    percentageValue,
    ')',
    ')?',
    '(?:',
    comma,
    ')?',
  ],
    searchFlags,
  )

  // Capture 1: url string contents
  // prettier-ignore
  const urlFunction = combineRegExp([
    'url\\(',
    optionalSpace,
    '(?:',
      '"',
      '(',
        '.*',
      ')',
      '"',
      '|',
      '\'',
      '(',
        '.*',
      ')',
      '\'',
      '|',
      '(',
        '.*',
      ')',
    ')',
    optionalSpace,
    '\\)'
  ],
    searchFlags,
  )

  // Capture 1: open comment, 2: inset, 3: offsetX, 4: offsetY, 5: blur radius, 6: spread radius, 7: trailing inset, 8: close comment
  // prettier-ignore
  const boxShadow = combineRegExp([
    openComment,
    optionalSpace,
    '(?:',
    '(',
    'inset',
    ')',
    space,
    ')?',
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')',
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')',
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')?',
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')?',
    color,
    '(?:',
    space,
    '(',
    'inset',
    ')',
    ')?',
    '(?:',
    comma,
    ')?',
    closeComment,
  ],
    searchFlags,
  )

  // Capture 1: openComment, 2: offsetX, 3: offsetY, 4: blur radius, 5: color, 6: close comment
  // prettier-ignore
  const textShadow = combineRegExp([
    openComment,
    optionalSpace,
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')',
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')',
    '(?:',
    '(',
    cssPixelValue,
    ')',
    space,
    ')?',
    color,
    optionalSpace,
    '(?:',
    comma,
    ')?',
    closeComment,
  ],
    searchFlags,
  )

  // prettier-ignore
  const transformItem = combineRegExp([
  '(\\/\\*)?',
  '(',
    cssTransformSupportedValidFunctionNames.join('|'),
  ')',
  '\\(',
  '(' ,
    '.*?',
  ')',
  '\\)',
  '(\\*\\/)?',
], searchFlags)

  return {
    comma,
    openComment,
    closeComment,
    optionalSpace,
    space,
    colorHex,
    colorHexOptionalOctothorp,
    searchFlags,
    angle,
    colorHSLa,
    colorRGBa,
    colorHSLaComponents,
    colorRGBaComponents,
    colorKeyword,
    color,
    colorComponents,
    cssPixelValue,
    percentageValue,
    gradientColorStop,
    gradientColorStops,
    gradientPosition,
    linearGradient,
    radialGradient,
    conicGradient,
    gradientColorStopValues,
    boxShadow,
    textShadow,
    transformItem,
    urlFunction,
  }
})()

export type PercentUnit = '%'

export type CSSNumberType =
  | 'Px'
  | 'Angle'
  | 'AnglePercent'
  | 'Length'
  | 'LengthPercent'
  | 'Percent'
  | 'Time'
  | 'TimePercent'
  | 'Resolution'
  | 'Unitless'
  | 'UnitlessPercent'
  | 'AnyValid'
  | 'Grid'

// https://developer.mozilla.org/en-US/docs/Web/CSS/length
export type FontRelativeLengthUnit = 'cap' | 'ch' | 'em' | 'ex' | 'ic' | 'lh' | 'rem' | 'rlh'
export type ViewportPercentageLengthUnit = 'vh' | 'vw' | 'vi' | 'vb' | 'vmin' | 'vmax'
export type AbsoluteLengthUnit = 'px' | 'cm' | 'mm' | 'Q' | 'in' | 'pc' | 'pt'
export type FlexibleLengthUnit = 'fr' // https://www.w3.org/TR/css3-grid-layout/#fr-unit
export type LengthUnit =
  | FontRelativeLengthUnit
  | ViewportPercentageLengthUnit
  | AbsoluteLengthUnit
  | FlexibleLengthUnit

const FontRelativeLengthUnits: Array<FontRelativeLengthUnit> = [
  'cap',
  'ch',
  'em',
  'ex',
  'ic',
  'lh',
  'rem',
  'rlh',
]
const ViewportPercentageLengthUnits: Array<ViewportPercentageLengthUnit> = [
  'vh',
  'vw',
  'vi',
  'vb',
  'vmin',
  'vmax',
]
export const AbsoluteLengthUnits: Array<AbsoluteLengthUnit> = [
  'px',
  'cm',
  'mm',
  'Q',
  'in',
  'pc',
  'pt',
]
export const FlexibleLengthUnits: Array<FlexibleLengthUnit> = ['fr']
export const LengthUnits: Array<LengthUnit> = [
  ...FontRelativeLengthUnits,
  ...ViewportPercentageLengthUnits,
  ...AbsoluteLengthUnits,
  ...FlexibleLengthUnits,
]

// https://developer.mozilla.org/en-US/docs/Web/CSS/length-percentage
export type LengthPercentUnit = LengthUnit | PercentUnit
export const LengthPercentUnits: Array<LengthPercentUnit> = [...LengthUnits, '%']

// https://developer.mozilla.org/en-US/docs/Web/CSS/angle
export type AngleUnit = 'deg' | 'grad' | 'rad' | 'turn'
const AngleUnits: Array<AngleUnit> = ['deg', 'grad', 'rad', 'turn']

// https://developer.mozilla.org/en-US/docs/Web/CSS/angle-percentage
export type AnglePercentUnit = AngleUnit | PercentUnit
const AnglePercentUnits: Array<AnglePercentUnit> = [...AngleUnits, '%']

// https://developer.mozilla.org/en-US/docs/Web/CSS/time
export type TimeUnit = 's' | 'ms'
const TimeUnits: Array<TimeUnit> = ['s', 'ms']

// https://developer.mozilla.org/en-US/docs/Web/CSS/time-percentage
export type TimePercentUnit = TimeUnit | PercentUnit
const TimePercentUnits: Array<TimePercentUnit> = [...TimeUnits, '%']

// https://developer.mozilla.org/en-US/docs/Web/CSS/resolution
export type ResolutionUnit = 'dpi' | 'dpcm' | 'dppx' | 'x'
const ResolutionUnits: Array<ResolutionUnit> = ['dpi', 'dpcm', 'dppx', 'x']

export type CSSNumberUnit = LengthUnit | AngleUnit | TimeUnit | ResolutionUnit | PercentUnit
const CSSNumberUnits: Array<CSSNumberUnit> = [
  ...LengthUnits,
  ...AngleUnits,
  ...TimeUnits,
  ...ResolutionUnits,
  '%',
]

export interface CSSNumber {
  value: number
  unit: CSSNumberUnit | null
}

export type GridCSSNumberUnit = LengthUnit | ResolutionUnit | PercentUnit | 'fr'
const GridCSSNumberUnits: Array<GridCSSNumberUnit> = [...LengthUnits, ...ResolutionUnits, '%', 'fr']

export interface GridCSSNumber {
  value: number
  unit: GridCSSNumberUnit | null
  areaName: string | null
}

export function gridCSSNumber(
  value: number,
  unit: GridCSSNumberUnit | null,
  areaName: string | null,
): GridCSSNumber {
  return {
    value: value,
    unit: unit,
    areaName: areaName,
  }
}

export function cssNumber(value: number, unit: CSSNumberUnit | null = null): CSSNumber {
  return { value, unit }
}

export function isCSSNumber(value: unknown): value is CSSNumber {
  return typeof value === 'object' && value != null && 'value' in value && 'unit' in value
}

export function getCSSNumberValue(value: CSSNumber | null | undefined): number | null {
  return value == null ? null : value.value
}

export function setCSSNumberValue(current: CSSNumber | null, newValue: number): CSSNumber {
  const currentUnit = current == null ? null : current.unit
  return cssNumber(newValue, currentUnit)
}

export function getCSSNumberUnit(current: CSSNumber | null): CSSNumberUnit | null {
  return current == null ? null : current.unit
}

export function isFixedSize(value: CSSNumber): boolean {
  if (value.unit == null) {
    return true
  } else {
    switch (value.unit) {
      case 'px':
      case 'cm':
      case 'mm':
      case 'Q':
      case 'in':
      case 'pc':
      case 'pt':
        return true
      default:
        return false
    }
  }
}

export function isCssNumberAndFixedSize(value: unknown): boolean {
  if (!isCSSNumber(value)) {
    return false
  }
  return isFixedSize(value)
}

export function isCssNumberAndPercentage(value: unknown): boolean {
  if (!isCSSNumber(value)) {
    return false
  }
  return value.unit === '%'
}

function parseCSSNumberUnit(
  input: string,
  units: Array<CSSNumberUnit>,
): Either<string, CSSNumberUnit> {
  if (units.includes(input as CSSNumberUnit)) {
    return right(input as CSSNumberUnit)
  } else {
    return left(`${input} is not a valid unit`)
  }
}

const parseCSSLengthUnit = (input: string) => parseCSSNumberUnit(input, LengthUnits)
const parseCSSLengthPercentUnit = (input: string) => parseCSSNumberUnit(input, LengthPercentUnits)
const parseCSSAngleUnit = (input: string) => parseCSSNumberUnit(input, AngleUnits)
const parseCSSAnglePercentUnit = (input: string) => parseCSSNumberUnit(input, AnglePercentUnits)
const parseCSSPxUnit = (input: string) => parseCSSNumberUnit(input, ['px'])
const parseCSSPercentUnit = (input: string) => parseCSSNumberUnit(input, ['%'])
const parseCSSTimeUnit = (input: string) => parseCSSNumberUnit(input, TimeUnits)
const parseCSSTimePercentUnit = (input: string) => parseCSSNumberUnit(input, TimePercentUnits)
const parseCSSResolutionUnit = (input: string) => parseCSSNumberUnit(input, ResolutionUnits)
const parseCSSUnitlessUnit = (_: string) => left<string, never>(`No unit expected`)
const parseCSSUnitlessPercentUnit = (input: string) => parseCSSNumberUnit(input, ['%'])
const parseCSSAnyValidNumberUnit = (input: string) => parseCSSNumberUnit(input, CSSNumberUnits)
const parseCSSGridUnit = (input: string) => parseCSSNumberUnit(input, GridCSSNumberUnits)

function unitParseFnForType(
  numberType: CSSNumberType,
): (input: string) => Either<string, CSSNumberUnit> {
  switch (numberType) {
    case 'Angle':
      return parseCSSAngleUnit
    case 'AnglePercent':
      return parseCSSAnglePercentUnit
    case 'Length':
      return parseCSSLengthUnit
    case 'LengthPercent':
      return parseCSSLengthPercentUnit
    case 'Px':
      return parseCSSPxUnit
    case 'Percent':
      return parseCSSPercentUnit
    case 'Resolution':
      return parseCSSResolutionUnit
    case 'Time':
      return parseCSSTimeUnit
    case 'TimePercent':
      return parseCSSTimePercentUnit
    case 'Unitless':
      return parseCSSUnitlessUnit
    case 'UnitlessPercent':
      return parseCSSUnitlessPercentUnit
    case 'AnyValid':
      return parseCSSAnyValidNumberUnit
    case 'Grid':
      return parseCSSGridUnit
    default:
      const _exhaustiveCheck: never = numberType
      throw new Error(`Unable to parse CSSNumber of type ${numberType}`)
  }
}

const LeadingSign = String.raw`(?:-?|\+?)`
const NumericPart = String.raw`(?:\d*\.?\d+|\d+\.?)`
const UnitPart = String.raw`[a-zA-Z%]*`
const NumericTypeStringRegex = new RegExp(
  String.raw`^\s*(${LeadingSign}${NumericPart})\s*(${UnitPart})\s*$`,
)

function parseCSSNumericTypeString(
  input: string,
  parseUnit: (i: string) => Either<string, CSSNumberUnit>,
  defaultUnit: CSSNumberUnit | null = null,
): Either<string, CSSNumber> {
  const matches = input.match(NumericTypeStringRegex)
  if (matches == null) {
    return left(`Unable to parse ${input}`)
  } else {
    const maybeValue = parseNumber(matches[1])
    return flatMapEither((value) => {
      const maybeUnit = matches[2]
      if (maybeUnit == null || maybeUnit === '') {
        return right({ value, unit: defaultUnit })
      } else {
        const parsedUnit = parseUnit(maybeUnit)
        return mapEither((unit) => cssNumber(value, unit), parsedUnit)
      }
    }, maybeValue)
  }
}

function fixNumber(n: number): number {
  return Number(n.toFixed(11))
}

export function printCSSNumber(
  input: CSSNumber,
  defaultUnitToSkip: string | null,
): string | number {
  const { value, unit } = input
  if (unit == null || unit === defaultUnitToSkip) {
    return fixNumber(value)
  } else {
    return `${fixNumber(value)}${unit}`
  }
}

export function printArrayCSSNumber(array: Array<GridCSSNumber>): string {
  return array
    .map((dimension) => {
      const printed = printCSSNumber(dimension, null)
      const areaName = dimension.areaName != null ? `[${dimension.areaName}] ` : ''
      return `${areaName}${printed}`
    })
    .join(' ')
}

export function printGridAutoOrTemplateBase(input: GridAutoOrTemplateBase): string {
  switch (input.type) {
    case 'DIMENSIONS':
      return printArrayCSSNumber(input.dimensions)
    case 'FALLBACK':
      return input.value
    default:
      assertNever(input)
  }
}

export function printCSSNumberOrKeyword(
  input: CSSNumber | CSSKeyword,
  defaultUnitToSkip: string | null,
): string | number {
  if (isCSSKeyword(input)) {
    return input.value
  } else {
    return printCSSNumber(input, defaultUnitToSkip)
  }
}

export function cssNumberToString(input: CSSNumber, showUnit: boolean = true): string {
  const printed = showUnit ? printCSSNumber(input, null) : fixNumber(input.value)
  return `${printed}`
}

export function printCSSNumberWithDefaultUnit(
  input: CSSNumber,
  defaultUnit: CSSNumberUnit,
): string {
  const { value, unit } = input
  const unitToUse = unit ?? defaultUnit
  return `${fixNumber(value)}${unitToUse}`
}

export const parseCSSLength = (input: unknown) => parseCSSNumber(input, 'Length')
export const parseCSSLengthPercent = (input: unknown) => parseCSSNumber(input, 'LengthPercent')
export const parseCSSLengthPercentNone = (
  input: unknown,
): Either<string, CSSNumber | undefined> => {
  if (typeof input === 'string' && input === 'none') {
    return right(undefined)
  } else {
    return parseCSSNumber(input, 'LengthPercent')
  }
}
export const parseCSSAngle = (input: unknown) => parseCSSNumber(input, 'Angle')
export const parseCSSAnglePercent = (input: unknown) => parseCSSNumber(input, 'AnglePercent')
export const parseCSSPx = (input: unknown) => parseCSSNumber(input, 'Px')
export const parseCSSPercent = (input: unknown) => parseCSSNumber(input, 'Percent', '%')
export const parseCSSResolution = (input: unknown) => parseCSSNumber(input, 'Resolution')
export const parseCSSTime = (input: unknown) => parseCSSNumber(input, 'Time')
export const parseCSSTimePercent = (input: unknown) => parseCSSNumber(input, 'TimePercent')
export const parseCSSUnitless = (input: unknown) => parseCSSNumber(input, 'Unitless')
export const parseCSSUnitlessPercent = (input: unknown) => parseCSSNumber(input, 'UnitlessPercent')
export const parseCSSAnyValidNumber = (input: unknown) => parseCSSNumber(input, 'AnyValid')
export const parseCSSGrid = (input: unknown) => parseCSSNumber(input, 'Grid')
export const parseCSSUnitlessAsNumber = (input: unknown): Either<string, number> => {
  const parsed = parseCSSNumber(input, 'Unitless')
  if (isRight(parsed)) {
    return right(parsed.value.value)
  } else {
    return parsed
  }
}

const gridCSSTemplateNumberRegex = /^\[(.+)\]\s*(.+)$/

export function parseToCSSGridNumber(input: unknown): Either<string, GridCSSNumber> {
  function getParts() {
    if (typeof input === 'string') {
      const match = input.match(gridCSSTemplateNumberRegex)
      if (match != null) {
        return {
          areaName: match[1],
          inputToParse: match[2],
        }
      }
    }
    return { areaName: null, inputToParse: input }
  }
  const { areaName, inputToParse } = getParts()

  return mapEither((value) => {
    return {
      value: value.value,
      unit: value.unit as GridCSSNumberUnit | null,
      areaName: areaName,
    }
  }, parseCSSGrid(inputToParse))
}

export const parseCSSNumber = (
  input: unknown,
  numberType: CSSNumberType,
  defaultUnit: CSSNumberUnit | null = null,
): Either<string, CSSNumber> => {
  if (typeof input === 'number' && (defaultUnit == null || defaultUnit === 'px')) {
    return right(cssNumber(input, defaultUnit))
  } else if (typeof input === 'string') {
    const unitParseFn = unitParseFnForType(numberType)
    return parseCSSNumericTypeString(input, unitParseFn, defaultUnit)
  } else {
    return left(`Unable to parse invalid number`)
  }
}

export function parseGridPosition(
  container: GridContainerProperties,
  axis: 'row' | 'column',
  edge: 'start' | 'end',
  shorthand: GridPosition | null,
  input: unknown,
): Either<string, GridPosition> {
  if (input === 'auto') {
    return right('auto')
  } else if (typeof input === 'string') {
    const referenceTemplate =
      axis === 'row' ? container.gridTemplateRows : container.gridTemplateColumns
    if (referenceTemplate?.type === 'DIMENSIONS') {
      const maybeArea = referenceTemplate.dimensions.findIndex((dim) => dim.areaName === input)
      if (maybeArea >= 0) {
        let value = gridPositionValue(maybeArea + 1)
        if (
          edge === 'end' &&
          shorthand != null &&
          shorthand !== 'auto' &&
          shorthand.numericalPosition === value.numericalPosition
        ) {
          value.numericalPosition = (value.numericalPosition ?? 0) + 1
        }
        return right(value)
      }
    }

    const asNumber = parseNumber(input)
    return mapEither(gridPositionValue, asNumber)
  } else if (typeof input === 'number') {
    return right(gridPositionValue(input))
  } else {
    return left('Not a valid grid position.')
  }
}

export function parseGridRange(
  container: GridContainerProperties,
  axis: 'row' | 'column',
  input: unknown,
): Either<string, GridRange> {
  if (typeof input === 'string') {
    if (input.includes('/')) {
      const splitInput = input.split('/')
      const startParsed = parseGridPosition(container, axis, 'start', null, splitInput[0])
      const endParsed = parseGridPosition(container, axis, 'end', null, splitInput[1])
      return applicative2Either(gridRange, startParsed, endParsed)
    } else {
      const startParsed = parseGridPosition(container, axis, 'start', null, input)
      return mapEither((start) => {
        const end =
          start !== 'auto' && start.numericalPosition != null
            ? gridPositionValue(start.numericalPosition + 1)
            : null
        return gridRange(start, end)
      }, startParsed)
    }
  } else {
    return left('Not a valid grid range.')
  }
}

export function expandRepeatFunctions(str: string): string {
  const node = parseCssTreeNodeValue(str)
  const expanded = expandCssTreeNodeValue(node)
  return csstree.generate(expanded)
}

const reGridAreaNameBrackets = /^\[.+\]$/

function normalizeGridTemplate(template: string): string {
  type normalizeFn = (s: string) => string

  const normalizePasses: normalizeFn[] = [
    // 1. expand repeat functions
    expandRepeatFunctions,
    // 2. normalize area names spacing
    (s) => s.replace(/\]/g, '] ').replace(/\[/g, ' ['),
  ]

  return normalizePasses.reduce((working, normalize) => normalize(working), template).trim()
}

export function tokenizeGridTemplate(template: string): string[] {
  let parts = normalizeGridTemplate(template).split(/\s+/)

  let tokens: string[] = []
  while (parts.length > 0) {
    const part = parts.shift()?.trim()
    if (part == null) {
      break
    }
    if (part.match(reGridAreaNameBrackets) != null && parts.length > 0) {
      const withAreaName = `${part} ${parts.shift()}`
      tokens.push(withAreaName)
    } else {
      tokens.push(part)
    }
  }
  return tokens
}

export function parseGridAutoOrTemplateBase(
  input: unknown,
): Either<string, GridAutoOrTemplateBase> {
  function numberParse(inputToParse: unknown): Either<ParseError, GridCSSNumber> {
    const result = parseToCSSGridNumber(inputToParse)
    return leftMapEither<string, ParseError, GridCSSNumber>(descriptionParseError, result)
  }
  if (typeof input === 'string') {
    const parsedCSSArray = parseCSSArray([numberParse])(tokenizeGridTemplate(input))
    return bimapEither(
      (error) => {
        if (error.type === 'DESCRIPTION_PARSE_ERROR') {
          return error.description
        } else {
          return error.toString()
        }
      },
      gridAutoOrTemplateDimensions,
      parsedCSSArray,
    )
  } else {
    return left('Unknown input.')
  }
}

export function parseDisplay(input: unknown): Either<string, string> {
  if (typeof input === 'string') {
    return right(input)
  } else {
    return left(`Unable to parse display value.`)
  }
}

export function framePinToCSSNumber(value: FramePin | undefined): CSSNumber | null {
  const parsed = parseCSSUnitlessPercent(value)
  return eitherToMaybe(parsed)
}

export function cssNumberToFramePin(value: CSSNumber): FramePin {
  if (value.unit == null) {
    return value.value
  } else {
    return printCSSNumber(value, 'px')
  }
}

export interface CSSPadding {
  paddingTop: CSSNumber
  paddingRight: CSSNumber
  paddingBottom: CSSNumber
  paddingLeft: CSSNumber
}

export interface CSSMargin {
  marginTop: CSSNumber
  marginRight: CSSNumber
  marginBottom: CSSNumber
  marginLeft: CSSNumber
}

export interface CSSFlex {
  flexGrow: number
  flexShrink: number
  flexBasis: CSSNumber
}

export function cssFlex(flexGrow: number, flexShrink: number, flexBasis: CSSNumber): CSSFlex {
  return {
    flexGrow: flexGrow,
    flexShrink: flexShrink,
    flexBasis: flexBasis,
  }
}

// For matching CSS Dimensions (lengths, angles etc.) as they are always specified as a number
// immediately followed by an optional unit (with no space between them)
const DimensionRegex = /(\-?[\d.]+)([a-zA-Z\%]*)/

export const WhitespaceRegex = /\s+/

export interface CSSDefault<T> {
  default: boolean
  value: T
}

export function cssDefault<T>(value: T, defaultValue = true): CSSDefault<T> {
  return {
    default: defaultValue,
    value,
  }
}

// ¯\_(ツ)_/¯
export function isCSSDefault<T>(value: unknown): value is CSSDefault<T> {
  return (
    typeof value === 'object' &&
    value != null &&
    'default' in value &&
    typeof (value as any).default === 'boolean' &&
    'value' in value
  )
}

export const defaultLengthUnit = 'px'

export const cssPixelLengthZero: CSSNumber = {
  value: 0,
  unit: 'px',
}

export const zeroAngle: CSSNumber = {
  value: 0,
  unit: 'deg',
}

export function cssPixelLength(length: number): CSSNumber {
  return {
    value: length,
    unit: 'px',
  }
}
export function cssPercentage(length: number): CSSNumber {
  return {
    value: length,
    unit: '%',
  }
}
export function cssAngle(value: number, unit: AnglePercentUnit = 'deg'): CSSNumber {
  return {
    value,
    unit,
  }
}
export function cssUnitlessLength(value: number): CSSNumber {
  return {
    value,
    unit: null,
  }
}

export function printEnabled(value: string, enabled: boolean): string {
  return `${enabled ? '' : '/*'}${value}${enabled ? '' : '*/'}`
}

export function parseEnabled(value: string): { value: string; enabled: boolean } {
  return { value, enabled: true }
}

export const defaultCSSColor: CSSColorHex = {
  type: 'Hex',
  hex: '#d3d3d3',
}
export const blackHexCSSColor: CSSColor = {
  type: 'Hex',
  hex: '#000',
}

export const blackHexTransparentCSSColor: CSSColor = {
  type: 'Hex',
  hex: '#0000',
}

export const whiteHexCSSColor: CSSColor = {
  type: 'Hex',
  hex: '#fff',
}

export interface CSSBoxShadow {
  type: 'box-shadow'
  enabled: boolean
  offsetX: CSSNumber
  offsetY: CSSNumber
  blurRadius: CSSDefault<CSSNumber>
  spreadRadius: CSSDefault<CSSNumber>
  inset: boolean
  color: CSSColor
}

function cssBoxShadow(
  enabled: boolean,
  offsetX: CSSNumber,
  offsetY: CSSNumber,
  blurRadius: CSSDefault<CSSNumber>,
  spreadRadius: CSSDefault<CSSNumber>,
  inset: boolean,
  color: CSSColor,
): CSSBoxShadow {
  return {
    type: 'box-shadow',
    enabled,
    offsetX,
    offsetY,
    blurRadius,
    spreadRadius,
    inset,
    color,
  }
}

export type CSSBoxShadows = ReadonlyArray<CSSBoxShadow>

export const cssLineWidthKeywordValues = ['thin', 'medium', 'thick'] as const
export type CSSLineWidthKeywordValue = NonNullable<(typeof cssLineWidthKeywordValues)[number]>
export type CSSLineWidthValue = CSSNumber | CSSKeyword<CSSLineWidthKeywordValue>
export interface CSSLineWidth {
  type: 'line-width'
  value: CSSLineWidthValue
}

export function cssLineWidth(value: CSSLineWidthValue): CSSLineWidth {
  return {
    type: 'line-width',
    value,
  }
}

export function isCSSLineWidth(
  value: CSSColor | CSSLineWidth | CSSLineStyle,
): value is CSSLineWidth {
  return value.type === 'line-width'
}

export const cssLineStyleKeywordValues = [
  'none',
  'hidden',
  'dotted',
  'dashed',
  'solid',
  'double',
  'groove',
  'ridge',
  'inset',
  'outset',
] as const
export type CSSLineStyleKeywordValue = NonNullable<(typeof cssLineStyleKeywordValues)[number]>
export type CSSLineStyleValue = CSSKeyword<CSSLineStyleKeywordValue>

export interface CSSLineStyle {
  type: 'line-style'
  value: CSSLineStyleValue
}

export function cssLineStyle(value: CSSLineStyleValue): CSSLineStyle {
  return {
    type: 'line-style',
    value,
  }
}

export function isCSSLineStyle(
  value: CSSColor | CSSLineWidth | CSSLineStyle,
): value is CSSLineWidth {
  return value.type === 'line-style'
}

export interface CSSBorder {
  type: 'border'
  style?: CSSLineStyle
  width?: CSSLineWidth
  color?: CSSColor
}

export const defaultBorderWidth = cssNumber(1, 'px')

export const emptyCSSBorder: CSSBorder = {
  type: 'border',
}

export const emptyCssBorderDefault: CSSBorder = {
  type: 'border',
  width: {
    type: 'line-width',
    value: {
      value: 0,
      unit: 'px',
    },
  },
  style: {
    type: 'line-style',
    value: {
      type: 'keyword',
      value: 'none',
    },
  },
  color: {
    type: 'RGB',
    r: 0,
    g: 0,
    b: 0,
    a: 1,
    percentageAlpha: false,
    percentagesUsed: false,
  },
}

export const defaultCSSBorder: Complete<CSSBorder> = {
  type: 'border',
  style: cssLineStyle(cssKeyword('solid')),
  width: cssLineWidth({ ...defaultBorderWidth }),
  color: { ...blackHexCSSColor },
}

function printBorder(value: CSSBorder): JSExpressionValue<string> {
  const color: string | null = value.color != null ? printColor(value.color) : null
  const width: string | null = (() => {
    if (value.width == null) {
      return null
    } else if (isCSSKeyword(value.width.value)) {
      return value.width.value.value
    } else {
      return cssNumberToString(value.width.value)
    }
  })()
  const style: CSSLineStyleKeywordValue | null = value.style?.value.value ?? null

  return jsExpressionValue(Utils.stripNulls([width, style, color]).join(' '), emptyComments)
}

export declare type Complete<T> = {
  [K in keyof T]-?: T[K]
}

export const defaultBoxShadow: CSSBoxShadow = {
  type: 'box-shadow',
  enabled: true,
  offsetX: cssPixelLength(0),
  offsetY: cssPixelLength(2),
  blurRadius: {
    default: false,
    value: cssPixelLength(4),
  },
  spreadRadius: {
    default: true,
    value: { ...cssPixelLengthZero },
  },
  inset: false,
  color: cssColorRGB(0, 0, 0, 0.12, false, false),
}

export const defaultBoxShadows: CSSBoxShadows = [{ ...defaultBoxShadow }]

export const disabledFunctionName = UtopiaUtils.disabled.name

export function printBoxShadow(boxShadows: CSSBoxShadows): JSExpressionValue<string> {
  const indexOfLastEnabledLayer = findLastIndex(isLayerEnabled, boxShadows)
  return jsExpressionValue(
    [...boxShadows]
      .map((boxShadow, i) => {
        const comma = indexOfLastEnabledLayer > i && boxShadow.enabled
        const { inset, offsetX, offsetY, blurRadius, spreadRadius, color } = boxShadow
        const parts = Utils.stripNulls([
          inset ? 'inset' : null,
          printCSSNumber(offsetX, null),
          printCSSNumber(offsetY, null),
          blurRadius.default && spreadRadius.default
            ? null
            : printCSSNumber(blurRadius.value, null),
          spreadRadius.default ? null : printCSSNumber(spreadRadius.value, null),
          printColor(color),
        ])
        return printEnabled(printComma(`${parts.join(' ')}`, comma), boxShadow.enabled)
      })
      .join(' '),
    emptyComments,
  )
}

export function parseBoxShadow(boxShadow: unknown): Either<string, CSSBoxShadows> {
  if (typeof boxShadow === 'string') {
    RegExpLibrary.boxShadow.lastIndex = 0
    let matches = RegExpLibrary.boxShadow.exec(boxShadow)
    let boxShadows: Array<CSSBoxShadow> = []
    while (matches != null) {
      const enabled = matches[1] == null && matches[9] == null
      const inset = matches[2] != null || matches[8] != null
      const parsedOffsetX = parseCSSLength(matches[3])
      const parsedOffsetY = parseCSSLength(matches[4])
      const parsedBlurRadius = parseCSSLength(matches[5])
      const blurRadius = cssDefault(
        isRight(parsedBlurRadius) ? parsedBlurRadius.value : { ...cssPixelLengthZero },
        !isRight(parsedBlurRadius),
      )
      const parsedSpreadRadius = parseCSSLength(matches[6])
      const spreadRadius = cssDefault(
        isRight(parsedSpreadRadius) ? parsedSpreadRadius.value : { ...cssPixelLengthZero },
        !isRight(parsedSpreadRadius),
      )
      const parsedColor = parseColor(matches[7], 'hex-hash-optional')
      if (isRight(parsedOffsetX) && isRight(parsedOffsetY) && isRight(parsedColor)) {
        const offsetX = parsedOffsetX.value
        const offsetY = parsedOffsetY.value
        const color = parsedColor.value
        boxShadows.push(
          cssBoxShadow(enabled, offsetX, offsetY, blurRadius, spreadRadius, inset, color),
        )
      }
      matches = RegExpLibrary.boxShadow.exec(boxShadow)
    }
    if (boxShadows.length > 0) {
      return right(boxShadows)
    }
  }
  return left('No box shadows found')
}

export interface CSSTransformRotateItem {
  enabled: boolean
  type: 'rotate'
  value: CSSNumber
}

export interface CSSTransformRotateXItem {
  enabled: boolean
  type: 'rotateX'
  value: CSSNumber
}
export interface CSSTransformRotateYItem {
  enabled: boolean
  type: 'rotateY'
  value: CSSNumber
}
export interface CSSTransformRotateZItem {
  enabled: boolean
  type: 'rotateZ'
  value: CSSNumber
}

export interface CSSTransformScaleItem {
  enabled: boolean
  type: 'scale'
  values: [CSSNumber, CSSDefault<CSSNumber>]
}
export interface CSSTransformScaleXItem {
  enabled: boolean
  type: 'scaleX'
  value: CSSNumber
}
export interface CSSTransformScaleYItem {
  enabled: boolean
  type: 'scaleY'
  value: CSSNumber
}
export interface CSSTransformScaleZItem {
  enabled: boolean
  type: 'scaleZ'
  value: CSSNumber
}
export interface CSSTransformSkewItem {
  enabled: boolean
  type: 'skew'
  values: [CSSNumber, CSSDefault<CSSNumber>]
}
export interface CSSTransformSkewXItem {
  enabled: boolean
  type: 'skewX'
  value: CSSNumber
}
export interface CSSTransformSkewYItem {
  enabled: boolean
  type: 'skewY'
  value: CSSNumber
}

export interface CSSTransformTranslateItem {
  enabled: boolean
  type: 'translate'
  values: [CSSNumber, CSSDefault<CSSNumber>]
}

export interface CSSTransformTranslateXItem {
  enabled: boolean
  type: 'translateX'
  value: CSSNumber
}

export interface CSSTransformTranslateYItem {
  enabled: boolean
  type: 'translateY'
  value: CSSNumber
}

export interface CSSTransformTranslateZItem {
  enabled: boolean
  type: 'translateZ'
  value: CSSNumber
}

export type CSSTransformItem =
  | CSSTransformRotateItem
  | CSSTransformRotateXItem
  | CSSTransformRotateYItem
  | CSSTransformRotateZItem
  | CSSTransformScaleItem
  | CSSTransformScaleXItem
  | CSSTransformScaleYItem
  | CSSTransformScaleZItem
  | CSSTransformSkewItem
  | CSSTransformSkewXItem
  | CSSTransformSkewYItem
  | CSSTransformTranslateItem
  | CSSTransformTranslateXItem
  | CSSTransformTranslateYItem
  | CSSTransformTranslateZItem
  | CSSUnknownArrayItem
export type CSSTransforms = ReadonlyArray<CSSTransformItem>

export function cssTransformRotate(
  angle: CSSNumber,
  enabled: boolean = true,
): CSSTransformRotateItem {
  return {
    type: 'rotate',
    enabled,
    value: angle,
  }
}

export function cssTransformRotateX(
  angle: CSSNumber,
  enabled: boolean = true,
): CSSTransformRotateXItem {
  return {
    type: 'rotateX',
    enabled,
    value: angle,
  }
}

export function cssTransformRotateY(
  angle: CSSNumber,
  enabled: boolean = true,
): CSSTransformRotateYItem {
  return {
    type: 'rotateY',
    enabled,
    value: angle,
  }
}

export function cssTransformRotateZ(
  angle: CSSNumber,
  enabled: boolean = true,
): CSSTransformRotateZItem {
  return {
    type: 'rotateZ',
    enabled,
    value: angle,
  }
}

export function cssTransformScale(
  x: CSSNumber,
  y: CSSDefault<CSSNumber> = cssDefault(x, true),
  enabled: boolean = true,
): CSSTransformScaleItem {
  return {
    type: 'scale',
    enabled,
    values: [x, y],
  }
}

export function cssTransformScaleX(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformScaleXItem {
  return {
    type: 'scaleX',
    enabled,
    value: length,
  }
}

export function cssTransformScaleY(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformScaleYItem {
  return {
    type: 'scaleY',
    enabled,
    value: length,
  }
}

export function cssTransformScaleZ(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformScaleZItem {
  return {
    type: 'scaleZ',
    enabled,
    value: length,
  }
}

export function cssTransformSkew(
  x: CSSNumber,
  y: CSSDefault<CSSNumber> = cssDefault(x, true),
  enabled: boolean = true,
): CSSTransformSkewItem {
  return {
    type: 'skew',
    enabled,
    values: [x, y],
  }
}

export function cssTransformSkewX(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformSkewXItem {
  return {
    type: 'skewX',
    enabled,
    value: length,
  }
}

export function cssTransformSkewY(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformSkewYItem {
  return {
    type: 'skewY',
    enabled,
    value: length,
  }
}

export function cssTransformTranslate(
  x: CSSNumber,
  y: CSSDefault<CSSNumber> = cssDefault(cssPixelLength(0), true),
  enabled: boolean = true,
): CSSTransformTranslateItem {
  return {
    type: 'translate',
    enabled,
    values: [x, y],
  }
}

export function cssTransformTranslateX(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformTranslateXItem {
  return {
    type: 'translateX',
    enabled,
    value: length,
  }
}

export function cssTransformTranslateY(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformTranslateYItem {
  return {
    type: 'translateY',
    enabled,
    value: length,
  }
}

export function cssTransformTranslateZ(
  length: CSSNumber,
  enabled: boolean = true,
): CSSTransformTranslateZItem {
  return {
    type: 'translateZ',
    enabled,
    value: length,
  }
}

export const defaultTransformRotate = cssTransformRotate(cssAngle(0))
export const defaultTransformRotateX = cssTransformRotateX(cssAngle(0))
export const defaultTransformRotateY = cssTransformRotateY(cssAngle(0))
export const defaultTransformRotateZ = cssTransformRotateZ(cssAngle(0))
export const defaultTransformScale = cssTransformScale(cssUnitlessLength(1))
export const defaultTransformScaleX = cssTransformScaleX(cssUnitlessLength(1))
export const defaultTransformScaleY = cssTransformScaleY(cssUnitlessLength(1))
export const defaultTransformScaleZ = cssTransformScaleZ(cssUnitlessLength(1))
export const defaultTransformSkew = cssTransformSkew(cssAngle(0), cssDefault(cssAngle(0)))
export const defaultTransformSkewX = cssTransformSkewX(cssAngle(0))
export const defaultTransformSkewY = cssTransformSkewY(cssAngle(0))
export const defaultTransformTranslate = cssTransformTranslate(
  cssUnitlessLength(0),
  cssDefault(cssUnitlessLength(0)),
)
export const defaultTransformTranslateX = cssTransformTranslateX(cssPixelLength(0))
export const defaultTransformTranslateY = cssTransformTranslateY(cssPixelLength(0))
export const defaultTransformTranslateZ = cssTransformTranslateZ(cssPixelLength(0))

export const defaultCSSTransforms: CSSTransforms = []

type SingleProducerFunction = (value: CSSNumber, enabled: boolean) => CSSTransformItem
const transformItemSingleHelpers: {
  [key in CSSTransformSingleSupportedType]: {
    parser: (value: string) => Either<string, CSSNumber>
    producer: SingleProducerFunction
  }
} = {
  rotate: {
    parser: parseCSSAngle,
    producer: cssTransformRotate,
  },
  rotateX: {
    parser: parseCSSAngle,
    producer: cssTransformRotateX,
  },
  rotateY: {
    parser: parseCSSAngle,
    producer: cssTransformRotateY,
  },
  rotateZ: {
    parser: parseCSSAngle,
    producer: cssTransformRotateZ,
  },
  scaleX: {
    parser: parseCSSUnitless,
    producer: cssTransformScaleX,
  },
  scaleY: {
    parser: parseCSSUnitless,
    producer: cssTransformScaleY,
  },
  scaleZ: {
    parser: parseCSSUnitless,
    producer: cssTransformScaleZ,
  },
  skewX: {
    parser: parseCSSAngle,
    producer: cssTransformSkewX,
  },
  skewY: {
    parser: parseCSSAngle,
    producer: cssTransformSkewY,
  },
  translateX: {
    parser: parseCSSLengthPercent,
    producer: cssTransformTranslateX,
  },
  translateY: {
    parser: parseCSSLengthPercent,
    producer: cssTransformTranslateY,
  },
  translateZ: {
    parser: parseCSSLengthPercent,
    producer: cssTransformTranslateZ,
  },
}

type DoubleProducerFunction = (
  value0: CSSNumber,
  value1: CSSDefault<CSSNumber>,
  enabled: boolean,
) => CSSTransformItem
const transformItemDoubleHelpers: {
  [key in CSSTransformDoubleSupportedType]: {
    parser: (value: string) => Either<string, CSSNumber>
    producer: DoubleProducerFunction
  }
} = {
  skew: {
    parser: parseCSSAngle,
    producer: cssTransformSkew,
  },
  scale: {
    parser: parseCSSUnitless,
    producer: cssTransformScale,
  },
  translate: {
    parser: parseCSSLengthPercent,
    producer: cssTransformTranslate,
  },
}

export type CSSTransformSingleLengthTypeItem =
  | 'translateX'
  | 'translateY'
  | 'translateZ'
  | 'rotate'
  | 'rotateX'
  | 'rotateY'
  | 'rotateZ'
  | 'scaleX'
  | 'scaleY'
  | 'scaleZ'
  | 'skewX'
  | 'skewY'

function isCSSTransformSingleItemType(type: string): type is CSSTransformSingleLengthTypeItem {
  return (
    type === 'translateX' ||
    type === 'translateY' ||
    type === 'translateZ' ||
    type === 'rotate' ||
    type === 'rotateX' ||
    type === 'rotateY' ||
    type === 'rotateZ' ||
    type === 'scaleX' ||
    type === 'scaleY' ||
    type === 'scaleZ' ||
    type === 'skewX' ||
    type === 'skewY'
  )
}

type CSSTransformDoubleLengthTypeItem = 'translate' | 'scale' | 'skew'

function isCSSTransformDoubleItemType(type: string): type is CSSTransformDoubleLengthTypeItem {
  return type === 'translate' || type === 'scale' || type === 'skew'
}

export type CSSTransformSingleLengthItem =
  | CSSTransformTranslateXItem
  | CSSTransformTranslateYItem
  | CSSTransformTranslateZItem
  | CSSTransformRotateItem
  | CSSTransformRotateXItem
  | CSSTransformRotateYItem
  | CSSTransformRotateZItem
  | CSSTransformScaleXItem
  | CSSTransformScaleYItem
  | CSSTransformScaleZItem
  | CSSTransformSkewXItem
  | CSSTransformSkewYItem

export function isCSSTransformSingleItem(
  cssTransformItem: CSSTransformItem,
): cssTransformItem is CSSTransformSingleLengthItem {
  return isCSSTransformSingleItemType(cssTransformItem.type)
}

export type CSSTransformDoubleLengthItem =
  | CSSTransformTranslateItem
  | CSSTransformScaleItem
  | CSSTransformSkewItem

export function isCSSTransformDoubleItem(
  cssTransformItem: CSSTransformItem,
): cssTransformItem is CSSTransformDoubleLengthItem {
  return isCSSTransformDoubleItemType(cssTransformItem.type)
}

export function parseTransform(transform: unknown): Either<string, CSSTransforms> {
  if (typeof transform === 'string') {
    const cssTransformsValue: Array<CSSTransformItem> = []
    RegExpLibrary.transformItem.lastIndex = 0
    let transformItemMatch = RegExpLibrary.transformItem.exec(transform)

    while (transformItemMatch != null) {
      const enabled = transformItemMatch[1] === undefined && transformItemMatch[4] === undefined
      const functionType = transformItemMatch[2]
      const functionParameters = transformItemMatch[3]
      if (isCSSTransformSingleItemType(functionType)) {
        const parsedValue = transformItemSingleHelpers[functionType].parser(functionParameters)
        if (isRight(parsedValue)) {
          const helper = transformItemSingleHelpers[functionType]
          cssTransformsValue.push(helper.producer(parsedValue.value, enabled))
        } else {
          cssTransformsValue.push(cssUnknownArrayItem(transformItemMatch[1]))
        }
      } else if (isCSSTransformDoubleItemType(functionType)) {
        const parameterArray = functionParameters.split(',')
        let parseError = false
        let parsedParameters: Array<CSSNumber | string> = parameterArray.map((parameter) => {
          const trimmed = parameter.trim()
          const parsedValue = transformItemDoubleHelpers[functionType].parser(trimmed)
          if (isLeft(parsedValue)) {
            parseError = true
          }
          return parsedValue.value
        })
        if (parseError) {
          cssTransformsValue.push(cssUnknownArrayItem(transformItemMatch[0]))
        } else {
          if (parsedParameters.length === 2) {
            cssTransformsValue.push(
              transformItemDoubleHelpers[functionType].producer(
                { ...(parsedParameters as Array<CSSNumber>)[0] },
                cssDefault({ ...(parsedParameters as Array<CSSNumber>)[1] }, false),
                enabled,
              ),
            )
          } else if (parsedParameters.length === 1) {
            if (functionType === 'translate') {
              cssTransformsValue.push(
                transformItemDoubleHelpers[functionType].producer(
                  { ...(parsedParameters as Array<CSSNumber>)[0] },
                  cssDefault(cssPixelLength(0)),
                  enabled,
                ),
              )
            } else {
              cssTransformsValue.push(
                transformItemDoubleHelpers[functionType].producer(
                  { ...(parsedParameters as Array<CSSNumber>)[0] },
                  cssDefault({ ...(parsedParameters as Array<CSSNumber>)[0] }, true),
                  enabled,
                ),
              )
            }
          } else {
            cssTransformsValue.push(cssUnknownArrayItem(transformItemMatch[0]))
          }
        }
      } else {
        cssTransformsValue.push(cssUnknownArrayItem(transformItemMatch[0]))
      }
      transformItemMatch = RegExpLibrary.transformItem.exec(transform)
    }
    if (cssTransformsValue.length > 0) {
      return right(cssTransformsValue)
    }
  }
  return left('No transforms found')
}

function printCSSTransformItem(cssTransform: CSSTransformItem): string {
  if (isCSSTransformSingleItem(cssTransform)) {
    return printEnabled(
      `${cssTransform.type}(${printCSSNumber(cssTransform.value, null)})`,
      cssTransform.enabled,
    )
  } else if (isCSSTransformDoubleItem(cssTransform)) {
    if (cssTransform.values[1].default) {
      return printEnabled(
        `${cssTransform.type}(${printCSSNumber(cssTransform.values[0], null)})`,
        cssTransform.enabled,
      )
    } else {
      return printEnabled(
        `${cssTransform.type}(${printCSSNumber(cssTransform.values[0], null)}, ${printCSSNumber(
          cssTransform.values[1].value,
          null,
        )})`,
        cssTransform.enabled,
      )
    }
  } else {
    return printEnabled(cssTransform.value, cssTransform.enabled)
  }
}

function printTransform(cssTransforms: CSSTransforms): JSExpressionValue<Property.Transform> {
  return jsExpressionValue(cssTransforms.map(printCSSTransformItem).join(' '), emptyComments)
}

export enum CSSTransformOriginStringValueX {
  Left = 'left',
  Center = 'center',
  Right = 'right',
}

export enum CSSTransformOriginStringValueY {
  Top = 'top',
  Center = 'center',
  Bottom = 'bottom',
}

export interface CSSTransformOrigin {
  x: CSSNumber | CSSTransformOriginStringValueX
  y: CSSNumber | CSSTransformOriginStringValueY
}

function parseTransformOrigin(transformOrigin: unknown): Either<string, CSSTransformOrigin> {
  const failureMsg = `Unable to parse transform origin ${transformOrigin}`
  if (typeof transformOrigin === 'string') {
    const parts = transformOrigin.split(WhitespaceRegex)
    const parsedPercentageLength0 = parseCSSLengthPercent(parts[0])
    if (parts.length == 1) {
      if (isRight(parsedPercentageLength0)) {
        return right({
          x: parsedPercentageLength0.value,
          y: parsedPercentageLength0.value,
        })
      } else {
        switch (parts[0]) {
          case 'left':
            return right({
              x: CSSTransformOriginStringValueX.Left,
              y: CSSTransformOriginStringValueY.Center,
            })
          case 'right':
            return right({
              x: CSSTransformOriginStringValueX.Right,
              y: CSSTransformOriginStringValueY.Center,
            })
          case 'top':
            return right({
              x: CSSTransformOriginStringValueX.Center,
              y: CSSTransformOriginStringValueY.Top,
            })
          case 'bottom':
            return right({
              x: CSSTransformOriginStringValueX.Center,
              y: CSSTransformOriginStringValueY.Bottom,
            })
          case 'center':
            return right({
              x: CSSTransformOriginStringValueX.Center,
              y: CSSTransformOriginStringValueY.Center,
            })
          default:
            return left(failureMsg)
        }
      }
    } else if (parts.length == 2) {
      const parsedPercentageLength1 = parseCSSLengthPercent(parts[1])
      if (isRight(parsedPercentageLength0) && isRight(parsedPercentageLength1)) {
        return right({
          x: parsedPercentageLength0.value,
          y: parsedPercentageLength1.value,
        })
      } else {
        let parsedX: CSSTransformOriginStringValueX
        let parsedY: CSSTransformOriginStringValueY
        switch (parts[0]) {
          case 'left':
            parsedX = CSSTransformOriginStringValueX.Left
            break
          case 'right':
            parsedX = CSSTransformOriginStringValueX.Right
            break
          case 'center':
            parsedX = CSSTransformOriginStringValueX.Center
            break
          default:
            return left(failureMsg)
        }

        switch (parts[1]) {
          case 'top':
            parsedY = CSSTransformOriginStringValueY.Top
            break
          case 'bottom':
            parsedY = CSSTransformOriginStringValueY.Bottom
            break
          case 'center':
            parsedY = CSSTransformOriginStringValueY.Center
            break
          default:
            return left(failureMsg)
        }
        return right({ x: parsedX, y: parsedY })
      }
    } else {
      return left(failureMsg)
    }
  } else {
    return left(failureMsg)
  }
}

function cssTransformOriginStringValueToPercentage(
  origin: CSSTransformOriginStringValueX | CSSTransformOriginStringValueY,
): number {
  switch (origin) {
    case CSSTransformOriginStringValueX.Left:
    case CSSTransformOriginStringValueY.Top:
      return 0
    case CSSTransformOriginStringValueX.Center:
    case CSSTransformOriginStringValueY.Center:
      return 0.5
    case CSSTransformOriginStringValueX.Right:
    case CSSTransformOriginStringValueY.Bottom:
      return 1
    default:
      const _exhaustiveCheck: never = origin
      throw new Error(`Unknown origin ${origin}`)
  }
}

export function normalisedCSSTransformOriginValueToCSSTransformValue(
  normilisedOrigin: number,
  axis: 'x',
): CSSTransformOriginStringValueX | CSSNumber

export function normalisedCSSTransformOriginValueToCSSTransformValue(
  normilisedOrigin: number,
  axis: 'y',
): CSSTransformOriginStringValueY | CSSNumber

export function normalisedCSSTransformOriginValueToCSSTransformValue(
  normilisedOrigin: number,
  axis: 'x' | 'y',
): CSSTransformOriginStringValueX | CSSTransformOriginStringValueY | CSSNumber

export function normalisedCSSTransformOriginValueToCSSTransformValue(
  normilisedOrigin: number,
  axis: 'x' | 'y',
): CSSTransformOriginStringValueX | CSSTransformOriginStringValueY | CSSNumber {
  if (axis === 'x') {
    switch (normilisedOrigin) {
      case 0:
        return CSSTransformOriginStringValueX.Left
      case 0.5:
        return CSSTransformOriginStringValueX.Center
      case 1:
        return CSSTransformOriginStringValueX.Right
      default:
        return {
          value: normilisedOrigin * 100,
          unit: '%',
        }
    }
  } else {
    switch (normilisedOrigin) {
      case 0:
        return CSSTransformOriginStringValueY.Top
      case 0.5:
        return CSSTransformOriginStringValueY.Center
      case 1:
        return CSSTransformOriginStringValueY.Bottom
      default:
        return {
          value: normilisedOrigin * 100,
          unit: '%',
        }
    }
  }
}

export function cssTransformOriginToNormalisedValue(cssTransformOrigin: CSSTransformOrigin): {
  x: number
  y: number
} {
  const x =
    typeof cssTransformOrigin.x === 'string'
      ? cssTransformOriginStringValueToPercentage(cssTransformOrigin.x)
      : cssTransformOrigin.x.value / 100
  const y =
    typeof cssTransformOrigin.y === 'string'
      ? cssTransformOriginStringValueToPercentage(cssTransformOrigin.y)
      : cssTransformOrigin.y.value / 100
  return { x, y }
}

function printTransformOriginComponent(
  transformOriginComponent:
    | CSSNumber
    | CSSTransformOriginStringValueX
    | CSSTransformOriginStringValueY,
): string {
  if (typeof transformOriginComponent === 'string') {
    return transformOriginComponent
  } else {
    return cssNumberToString(transformOriginComponent)
  }
}

function printTransformOrigin(transformOrigin: CSSTransformOrigin): JSExpressionValue<string> {
  return jsExpressionValue(
    `${printTransformOriginComponent(transformOrigin.x)} ${printTransformOriginComponent(
      transformOrigin.y,
    )}`,
    emptyComments,
  )
}

type CSSOverflow = boolean

function parseOverflow(overflow: unknown): Either<string, CSSOverflow> {
  if (typeof overflow === 'string') {
    return right(overflow !== 'hidden' && overflow !== 'clip')
  } else {
    return left('Invalid overflow')
  }
}

function printOverflow(overflow: CSSOverflow): JSExpressionValue<string> {
  return jsExpressionValue(overflow ? 'visible' : 'hidden', emptyComments)
}

export interface CSSBorderRadiusIndividual {
  tl: CSSNumber
  tr: CSSNumber
  br: CSSNumber
  bl: CSSNumber
}

export const defaultBorderRadiusIndividual: CSSBorderRadiusIndividual = {
  tl: { ...cssPixelLengthZero },
  tr: { ...cssPixelLengthZero },
  br: { ...cssPixelLengthZero },
  bl: { ...cssPixelLengthZero },
}

export type CSSBorderRadius = Either<CSSNumber, CSSBorderRadiusIndividual>

export function parseBorderRadius(borderRadius: unknown): Either<string, CSSBorderRadius> {
  const failureMsg = `Unable to parse border radius ${borderRadius}`
  if (typeof borderRadius === 'number') {
    return right(left({ value: borderRadius, unit: defaultLengthUnit }))
  } else if (borderRadius != null && typeof borderRadius === 'string') {
    if (borderRadius.indexOf('/') >= 0) {
      return left(failureMsg)
    } else {
      const partsEither = traverseEither(
        (part) => parseCSSLengthPercent(part),
        borderRadius.split(WhitespaceRegex),
      )
      if (isRight(partsEither)) {
        const parts = partsEither.value
        if (parts.length === 1) {
          // value is applied to all corners
          return right(left(parts[0]))
        } else if (parts.length === 2) {
          // 1st is top left and bottom right, 2nd is top right and bottom left
          return right(
            right({
              tl: parts[0],
              tr: parts[1],
              br: parts[0],
              bl: parts[1],
            }),
          )
        } else if (parts.length === 3) {
          // 1st is top left, 2nd is top right and bottom left, 3rd is bottom right. Because CSS
          return right(
            right({
              tl: parts[0],
              tr: parts[1],
              br: parts[2],
              bl: parts[1],
            }),
          )
        } else if (parts.length === 4) {
          return right(
            right({
              tl: parts[0],
              tr: parts[1],
              br: parts[2],
              bl: parts[3],
            }),
          )
        } else {
          return left(failureMsg)
        }
      } else {
        return left(failureMsg)
      }
    }
  } else {
    return left(failureMsg)
  }
}

export function printBorderRadius(
  borderRadius: CSSBorderRadius,
): JSExpressionValue<string | number> {
  if (isLeft(borderRadius)) {
    return printCSSNumberAsAttributeValue('px')(borderRadius.value)
  } else {
    const { tl, tr, br, bl } = borderRadius.value
    return jsExpressionValue(
      `${printCSSNumber(tl, null)} ${printCSSNumber(tr, null)} ${printCSSNumber(
        br,
        null,
      )} ${printCSSNumber(bl, null)}`,
      emptyComments,
    )
  }
}

export interface CSSColorKeyword {
  type: 'Keyword'
  keyword: string
}

export interface CSSColorHex {
  type: 'Hex'
  hex: string
}

export interface CSSColorHSL {
  type: 'HSL'
  h: number
  s: number
  l: number
  a: number
  percentageAlpha: boolean
}

export function cssColorHSL(
  h: number,
  s: number,
  l: number,
  a: number,
  percentageAlpha: boolean,
): CSSColorHSL {
  return {
    type: 'HSL',
    h,
    s,
    l,
    a,
    percentageAlpha,
  }
}

export interface CSSColorRGB {
  type: 'RGB'
  r: number
  g: number
  b: number
  a: number
  percentageAlpha: boolean
  percentagesUsed: boolean
}

export function cssColorRGB(
  r: number,
  g: number,
  b: number,
  a: number,
  percentageAlpha: boolean,
  percentagesUsed: boolean,
): CSSColorRGB {
  return {
    type: 'RGB',
    r,
    g,
    b,
    a,
    percentageAlpha,
    percentagesUsed,
  }
}

export type CSSColor = CSSColorKeyword | CSSColorHex | CSSColorHSL | CSSColorRGB

const defaultAlpha: NumberOrPercent = { value: 1, isPercent: false }

export function isKeyword(color: CSSColor): color is CSSColorKeyword {
  return color.type === 'Keyword'
}

export function isHex(color: CSSColor): color is CSSColorHex {
  return color.type === 'Hex'
}

export function isHSL(color: CSSColor): color is CSSColorHSL {
  return color.type === 'HSL'
}

export function isRGB(color: CSSColor): color is CSSColorRGB {
  return color.type === 'RGB'
}

export function isCSSColor(value: unknown): value is CSSColor {
  return (
    typeof value === 'object' &&
    value != null &&
    ((value as any).type === 'Keyword' ||
      (value as any).type === 'Hex' ||
      (value as any).type === 'HSL' ||
      (value as any).type === 'RGB')
  )
}

export function cssColorToChromaColorOrDefault(
  c: CSSColor,
  defaultHex: string = defaultCSSColor.hex,
): Chroma.Color {
  const parsed = cssColorToChromaColor(c)
  if (isRight(parsed)) {
    return parsed.value
  } else {
    return Chroma(defaultHex)
  }
}

export function cssColorToChromaColor(c: CSSColor): Either<string, Chroma.Color> {
  if (isKeyword(c)) {
    if (Chroma.valid(c.keyword)) {
      return right(Chroma(c.keyword))
    } else {
      return left(`Color keyword ${c.keyword} is not a valid color.`)
    }
  } else if (isHex(c)) {
    return right(Chroma.hex(c.hex))
  } else if (isHSL(c)) {
    return right((Chroma.hsl as any)(c.h, c.s / 100.0, c.l / 100.0, c.a))
  } else {
    return right(Chroma(c.r, c.g, c.b, c.a))
  }
}

function separateParams(s: string): Either<string, Array<string>> {
  const failureMsg = `Unable to separate params of ${s}`
  const firstSplit = s.split('(')
  if (firstSplit.length > 1) {
    const secondSplit = firstSplit[1].split(')')
    if (secondSplit.length > 1) {
      const commaSplit = secondSplit[0].split(',')
      if (commaSplit.length > 1) {
        return right(commaSplit.map((param) => param.trim()))
      } else {
        const slashSplit = commaSplit[0].trim().split(/\//)
        if (slashSplit.length > 1) {
          return right([
            ...slashSplit[0]
              .trim()
              .split(/\s+/)
              .map((param) => param.trim()),
            slashSplit[1],
          ])
        } else {
          return right(
            commaSplit[0]
              .trim()
              .split(/\s+/)
              .map((param) => param.trim()),
          )
        }
      }
    } else {
      return left(failureMsg)
    }
  } else {
    return left(failureMsg)
  }
}

function parseRGBColor(params: string[]): Either<string, CSSColorRGB> {
  if (params.length >= 3) {
    const parsedParams = traverseEither(parseNumberOrPercent, params)
    return bimapEither(
      (l) => `RGB has invalid params ${JSON.stringify(params)} ${l}`,
      (r) => {
        const alpha = r.length >= 4 ? r[3] : { ...defaultAlpha }
        const percentagesUsed = r.slice(0, 2).some((v) => v.isPercent) // mixed percentages and numbers aren't allowed, so if one it they are all
        return {
          type: 'RGB',
          r: numberOrPercentToNumber(r[0], 255),
          g: numberOrPercentToNumber(r[1], 255),
          b: numberOrPercentToNumber(r[2], 255),
          a: numberOrPercentToNumber(alpha, 1),
          percentageAlpha: alpha.isPercent,
          percentagesUsed: percentagesUsed,
        }
      },
      parsedParams,
    )
  } else {
    return left(`RGB definition has insufficient params ${JSON.stringify(params)}`)
  }
}

function parseHSLColor(params: string[]): Either<string, CSSColorHSL> {
  if (params.length >= 3) {
    const h = parseNumber(params[0])
    const s = parseNumberOrPercent(params[1])
    const l = parseNumberOrPercent(params[2])
    const a = params.length >= 4 ? parseNumberOrPercent(params[3]) : right(defaultAlpha)
    if (isRight(h) && isRight(s) && isRight(l) && isRight(a)) {
      return right({
        type: 'HSL',
        h: h.value,
        s: s.value.value,
        l: l.value.value,
        a: numberOrPercentToNumber(a.value, 1),
        percentageAlpha: a.value.isPercent,
      })
    } else {
      return left(`HSL has invalid params ${JSON.stringify(params)}`)
    }
  } else {
    return left(`HSL definition has insufficient params ${JSON.stringify(params)}`)
  }
}

const solidColorRegExp = combineRegExp([
  RegExpLibrary.openComment,
  RegExpLibrary.color,
  RegExpLibrary.closeComment,
])

export function parseBackgroundColor(color?: unknown): Either<string, CSSDefault<CSSSolidColor>> {
  if (typeof color === 'string') {
    let parsed: Either<string, CSSColor>
    const matches = color.match(solidColorRegExp)
    if (matches != null) {
      parsed = parseColor(matches[2], 'hex-hash-optional')
      const enabled = matches[1] === undefined && matches[3] === undefined
      if (isRight(parsed)) {
        const underlyingColor = cssSolidColor(parsed.value, enabled)
        const isDefault = fastDeepEqual(underlyingColor, emptyBackgroundColor)
        return right(cssDefault(underlyingColor, isDefault))
      } else {
        return parsed
      }
    }
  }
  return left('No background color found')
}

function printBackgroundColor(value: CSSDefault<CSSSolidColor>): JSExpressionValue<string> {
  return jsExpressionValue(
    printEnabled(printColor(value.value.color), value.value.enabled),
    emptyComments,
  )
}

const matchColorKeyword = combineRegExp(['^', '(', RegExpLibrary.colorKeyword, ')', '$'])
const matchColorHex = combineRegExp(['^', RegExpLibrary.colorHexOptionalOctothorp, '$'])
const matchColorHexStrict = combineRegExp(['^', RegExpLibrary.colorHex, '$'])

function parseHexColor(
  color: string,
  strictHash: 'hex-hash-required' | 'hex-hash-optional',
): Either<string, CSSColor> {
  if (strictHash === 'hex-hash-required') {
    const matchedHex = color.match(matchColorHexStrict)
    if (Array.isArray(matchedHex)) {
      return right({
        type: 'Hex',
        hex: matchedHex[0],
      })
    }
  } else {
    const matchedHex = color.match(matchColorHex)
    if (Array.isArray(matchedHex) && matchedHex[2] != null) {
      return right({
        type: 'Hex',
        hex: '#' + matchedHex[2],
      })
    }
  }

  return left('Not a valid hex color.')
}

export function parseColor(
  color: unknown,
  strictHash: 'hex-hash-required' | 'hex-hash-optional',
): Either<string, CSSColor> {
  if (color == null) {
    return left('No color value provided.')
  }
  if (typeof color !== 'string') {
    return left('Value not valid.')
  }
  const trimmed = color.trim()

  if (trimmed.startsWith('rgb')) {
    return flatMapEither(parseRGBColor, separateParams(trimmed))
  }

  if (trimmed.startsWith('hsl')) {
    return flatMapEither(parseHSLColor, separateParams(trimmed))
  }

  const parsedHex = parseHexColor(trimmed, strictHash)
  if (isRight(parsedHex)) {
    return parsedHex
  }

  if (trimmed === 'transparent') {
    // Chroma falls on its arse when trying to deal with 'transparent'
    return right({ ...blackHexTransparentCSSColor })
  }

  const matchedKeyword = trimmed.match(matchColorKeyword)
  if (Array.isArray(matchedKeyword) && matchedKeyword[1] != null) {
    return right({
      type: 'Keyword',
      keyword: matchedKeyword[1],
    })
  }

  return left('No valid color found.')
}

const parseColorHexHashOptional = (color: unknown) => parseColor(color, 'hex-hash-optional')

export function printColorToJsx(color: CSSColor | undefined): JSExpressionValue<string> {
  return jsExpressionValue(color != null ? printColor(color) : '', emptyComments)
}

export function cssColor(value: string, defaultColor: CSSColor = { ...defaultCSSColor }): CSSColor {
  const parsedColor = parseColor(value, 'hex-hash-optional')
  if (isRight(parsedColor)) {
    return parsedColor.value
  } else {
    return defaultColor
  }
}

export function printColor(color: CSSColor): string {
  switch (color.type) {
    case 'Hex':
      return color.hex
    case 'Keyword':
      return color.keyword
    case 'RGB': {
      const toPercentage = (n: number) => `${(n * 100) / 255}%`
      const { percentageAlpha, percentagesUsed } = color
      const r = percentagesUsed ? toPercentage(color.r) : `${color.r}`
      const g = percentagesUsed ? toPercentage(color.g) : `${color.g}`
      const b = percentagesUsed ? toPercentage(color.b) : `${color.b}`
      const a = percentageAlpha ? `${color.a * 100}%` : `${color.a}`
      return `rgb(${r}, ${g}, ${b}, ${a})`
    }
    case 'HSL': {
      const { percentageAlpha, s, l } = color
      const h = color.h + ''
      const a = percentageAlpha ? `${color.a * 100}%` : `${color.a}`
      return `hsl(${h}, ${s}%, ${l}%, ${a})`
    }
    default:
      const _exhaustiveCheck: never = color
      throw new Error(`Unable to print color ${JSON.stringify(color)}`)
  }
}

export function parseAlphaFromCSSColor(color: CSSColor): number {
  switch (color.type) {
    case 'Hex':
      return Chroma.hex(color.hex).alpha()
    case 'Keyword':
      return color.keyword === 'transparent' ? 0 : 1
    case 'HSL':
    case 'RGB':
      return color.a
    default:
      const _exhaustiveCheck: never = color
      throw new Error('No valid alpha.')
  }
}

export function giveCSSColorNewAlpha(newAlpha: number, color: CSSColor): CSSColor {
  switch (color.type) {
    case 'Hex':
      return {
        type: 'Hex',
        hex: Chroma(color.hex).alpha(newAlpha).hex().toUpperCase(),
      }
    case 'Keyword':
      if (color.keyword === 'transparent') {
        return {
          type: 'RGB',
          r: 0,
          g: 0,
          b: 0,
          a: newAlpha,
          percentageAlpha: false,
          percentagesUsed: false,
        } as CSSColorRGB
      } else {
        const [r, g, b] = Chroma(color.keyword).rgba()
        return {
          type: 'RGB',
          r,
          g,
          b,
          a: newAlpha,
          percentageAlpha: false,
          percentagesUsed: false,
        } as CSSColorRGB
      }
    case 'HSL':
      return {
        ...color,
        a: newAlpha,
        percentageAlpha: false,
      } as CSSColorHSL
    case 'RGB':
      return {
        ...color,
        a: newAlpha,
        percentageAlpha: false,
      } as CSSColorRGB
    default:
      const _exhaustiveCheck: never = color
      throw new Error('No valid alpha.')
  }
}

export const zeroPercent: CSSNumber = {
  value: 0,
  unit: '%',
}

export const fiftyPercent: CSSNumber = {
  value: 50,
  unit: '%',
}

export const oneHundredPercent: CSSNumber = {
  value: 100,
  unit: '%',
}

export interface CSSGradientStop {
  color: CSSColor
  position: CSSNumber
}

export const defaultCSSGradientStops: Array<CSSGradientStop> = [
  {
    color: { ...blackHexCSSColor },
    position: { ...zeroPercent },
  },
  {
    color: { ...whiteHexCSSColor },
    position: { ...oneHundredPercent },
  },
]

export type CSSGradientAngleKeywords =
  | 'top'
  | 'right'
  | 'bottom'
  | 'left'
  | 'top right'
  | 'right top'
  | 'bottom right'
  | 'right bottom'
  | 'bottom left'
  | 'left bottom'
  | 'top left'
  | 'left top'

export type GradientBlendingMode = 'rgb' | 'hsl' | 'hsv' | 'hsi' | 'lab' | 'lch' | 'hcl'

type SolidBackgroundType = 'solid'
type ImageURLBackgroundType = 'url-function'
type GradientBackgroundType = 'linear-gradient' | 'radial-gradient' | 'conic-gradient'
type CSSBackgroundType = SolidBackgroundType | ImageURLBackgroundType | GradientBackgroundType

export type SolidBackgroundLayerType = 'solid-background-layer'
export type ImageURLBackgroundLayerType = 'url-function-background-layer'
export type GradientBackgroundLayerType =
  | 'linear-gradient-background-layer'
  | 'radial-gradient-background-layer'
  | 'conic-gradient-background-layer'
export type CSSBackgroundLayerType =
  | SolidBackgroundLayerType
  | ImageURLBackgroundLayerType
  | GradientBackgroundLayerType

export const cssBGSizeKeywordValueValues = ['contain', 'cover'] as const
export type CSSBGSizeKeywordValueValue = NonNullable<(typeof cssBGSizeKeywordValueValues)[number]>
export type CSSBGSizeKeywordValue = CSSKeyword<CSSBGSizeKeywordValueValue>
export type CSSBGSizeCurlyBraceValueValue = CSSNumber | CSSKeyword<'auto'>
export type CSSBGSizeCurlyBraceValue = ParsedCurlyBrace<CSSBGSizeCurlyBraceValueValue>

export type CSSBGSizeValue = CSSBGSizeKeywordValue | CSSBGSizeCurlyBraceValue

export interface CSSBGSize {
  type: 'bg-size'
  enabled: boolean
  size: CSSDefault<CSSBGSizeValue>
}

export function cssBGSize(size: CSSDefault<CSSBGSizeValue>, enabled = true): CSSBGSize {
  return {
    type: 'bg-size',
    enabled,
    size,
  }
}

export function isCSSBackgroundLayerWithBGSize(
  value: CSSBackgroundLayer,
): value is CSSGradientBackgroundLayer | CSSURLFunctionBackgroundLayer {
  return 'bgSize' in value
}

export const defaultBGSize = cssBGSize(cssDefault(parsedCurlyBrace([cssKeyword('auto')])), true)

export type CSSBackgroundSize = Array<CSSBGSize>

export type CSSBackground = CSSGradient | CSSSolidColor | CSSURLFunction | CSSUnknownArrayItem
export type CSSBackgrounds = ReadonlyArray<CSSBackground>

export interface CSSBackgroundLayerBase {
  bgSize: CSSBGSize
}

export interface CSSSolidBackgroundLayer extends SolidColorBase {
  type: SolidBackgroundLayerType
}
export function cssSolidBackgroundLayer(value: CSSSolidColor): CSSSolidBackgroundLayer {
  return {
    ...value,
    type: 'solid-background-layer',
  }
}
export function isCSSSolidBackgroundLayer(
  value: CSSBackgroundLayer,
): value is CSSSolidBackgroundLayer {
  return value.type === 'solid-background-layer'
}

export interface CSSLinearGradientBackgroundLayer
  extends CSSBackgroundLayerBase,
    LinearGradientBase {
  type: 'linear-gradient-background-layer'
}
export function cssLinearGradientBackgroundLayer(
  value: CSSLinearGradient,
  bgSize: CSSBGSize = { ...defaultBGSize },
): CSSLinearGradientBackgroundLayer {
  return {
    ...value,
    type: 'linear-gradient-background-layer',
    bgSize,
  }
}
function isCSSLinearGradientBackgroundLayer(
  value: CSSBackgroundLayer,
): value is CSSLinearGradientBackgroundLayer {
  return value.type === 'linear-gradient-background-layer'
}

export interface CSSRadialGradientBackgroundLayer
  extends CSSBackgroundLayerBase,
    RadialGradientBase {
  type: 'radial-gradient-background-layer'
}
function cssRadialGradientBackgroundLayer(
  value: CSSRadialGradient,
  bgSize: CSSBGSize = { ...defaultBGSize },
): CSSRadialGradientBackgroundLayer {
  return {
    ...value,
    type: 'radial-gradient-background-layer',
    bgSize,
  }
}
export function isCSSRadialGradientBackgroundLayer(
  value: CSSBackgroundLayer,
): value is CSSRadialGradientBackgroundLayer {
  return value.type === 'radial-gradient-background-layer'
}

export interface CSSConicGradientBackgroundLayer extends CSSBackgroundLayerBase, ConicGradientBase {
  type: 'conic-gradient-background-layer'
}
function cssConicGradientBackgroundLayer(
  value: CSSConicGradient,
  bgSize: CSSBGSize = { ...defaultBGSize },
): CSSConicGradientBackgroundLayer {
  return {
    ...value,
    type: 'conic-gradient-background-layer',
    bgSize,
  }
}
export function isCSSConicGradientBackgroundLayer(
  value: CSSBackgroundLayer,
): value is CSSConicGradientBackgroundLayer {
  return value.type === 'conic-gradient-background-layer'
}

export interface CSSURLFunctionBackgroundLayer extends CSSBackgroundLayerBase, URLFunctionBase {
  type: 'url-function-background-layer'
}

function cssURLFunctionBackgroundLayer(
  value: CSSURLFunction,
  bgSize: CSSBGSize = { ...defaultBGSize },
): CSSURLFunctionBackgroundLayer {
  return {
    ...value,
    type: 'url-function-background-layer',
    bgSize,
  }
}

export function isCSSImageURLBackgroundLayer(
  value: CSSBackgroundLayer,
): value is CSSURLFunctionBackgroundLayer {
  return value.type === 'url-function-background-layer'
}

export const emptyURLFunctionBackgroundLayer: CSSURLFunctionBackgroundLayer = {
  type: 'url-function-background-layer',
  enabled: true,
  url: '',
  bgSize: { ...defaultBGSize },
}

export type CSSGradientBackgroundLayer =
  | CSSLinearGradientBackgroundLayer
  | CSSRadialGradientBackgroundLayer
  | CSSConicGradientBackgroundLayer

export function isCSSGradientBackgroundLayer(
  cssBackgroundLayer: CSSBackgroundLayer,
): cssBackgroundLayer is CSSGradientBackgroundLayer {
  return (
    cssBackgroundLayer.type === 'linear-gradient-background-layer' ||
    cssBackgroundLayer.type === 'radial-gradient-background-layer' ||
    cssBackgroundLayer.type === 'conic-gradient-background-layer'
  )
}

export type CSSBackgroundImageLayer = CSSGradientBackgroundLayer | CSSURLFunctionBackgroundLayer

export function isCSSBackgroundImageLayer(
  cssBackgroundLayer: CSSBackgroundLayer,
): cssBackgroundLayer is CSSBackgroundImageLayer {
  return (
    cssBackgroundLayer.type === 'linear-gradient-background-layer' ||
    cssBackgroundLayer.type === 'radial-gradient-background-layer' ||
    cssBackgroundLayer.type === 'conic-gradient-background-layer' ||
    cssBackgroundLayer.type === 'url-function-background-layer'
  )
}

export type CSSBackgroundLayer =
  | CSSSolidBackgroundLayer
  | CSSGradientBackgroundLayer
  | CSSURLFunctionBackgroundLayer
  | CSSUnknownArrayItem

export type CSSBackgroundLayers = ReadonlyArray<CSSBackgroundLayer>

interface LinearGradientBase {
  enabled: boolean
  angle: CSSDefault<CSSNumber>
  stops: Array<CSSGradientStop>
}

export interface CSSLinearGradient extends LinearGradientBase {
  type: 'linear-gradient'
}

function cssLinearGradient(
  stops: Array<CSSGradientStop>,
  angle: CSSDefault<CSSNumber>,
  enabled: boolean = true,
): CSSLinearGradient {
  return {
    type: 'linear-gradient',
    angle,
    enabled,
    stops,
  }
}

export type CSSRadialGradientSize = {
  width: CSSDefault<CSSNumber>
  height: CSSDefault<CSSNumber>
}

export const defaultCSSRadialGradientSize: CSSRadialGradientSize = {
  width: cssDefault(cssPercentage(100)),
  height: cssDefault(cssPercentage(100)),
}

type CSSRadialOrConicGradientCenter = {
  x: CSSDefault<CSSNumber>
  y: CSSDefault<CSSNumber>
}

export const defaultCSSRadialOrConicGradientCenter: CSSRadialOrConicGradientCenter = {
  x: cssDefault({ ...fiftyPercent }),
  y: cssDefault({ ...fiftyPercent }),
}

interface RadialGradientBase {
  enabled: boolean
  gradientSize: CSSRadialGradientSize
  center: CSSRadialOrConicGradientCenter
  stops: Array<CSSGradientStop>
}

export interface CSSRadialGradient extends RadialGradientBase {
  type: 'radial-gradient'
}

function cssRadialGradient(
  stops: Array<CSSGradientStop>,
  gradientSize: CSSRadialGradientSize,
  center: CSSRadialOrConicGradientCenter,
  enabled: boolean = true,
): CSSRadialGradient {
  return {
    type: 'radial-gradient',
    enabled,
    stops,
    gradientSize,
    center,
  }
}

interface ConicGradientBase {
  enabled: boolean
  fromAngle: CSSDefault<CSSNumber>
  center: CSSRadialOrConicGradientCenter
  stops: Array<CSSGradientStop>
}

export interface CSSConicGradient extends ConicGradientBase {
  type: 'conic-gradient'
}
function cssConicGradient(
  stops: Array<CSSGradientStop>,
  fromAngle: CSSDefault<CSSNumber>,
  center: CSSRadialOrConicGradientCenter,
  enabled: boolean = true,
): CSSConicGradient {
  return {
    type: 'conic-gradient',
    enabled,
    stops,
    fromAngle,
    center,
  }
}

export type CSSGradient = CSSLinearGradient | CSSRadialGradient | CSSConicGradient

export function isCSSGradient(cssBackground: CSSBackground): cssBackground is CSSGradient {
  return (
    cssBackground.type === 'linear-gradient' ||
    cssBackground.type === 'radial-gradient' ||
    cssBackground.type === 'conic-gradient'
  )
}

export interface SolidColorBase {
  color: CSSColor
  enabled: boolean
}

export interface CSSSolidColor extends SolidColorBase {
  type: 'solid'
}

export function cssSolidColor(color: CSSColor, enabled = true): CSSSolidColor {
  return {
    type: 'solid',
    color,
    enabled,
  }
}

export const emptyBackgroundColor: CSSSolidColor = cssSolidColor(
  cssColorRGB(0, 0, 0, 0, false, false),
)

export function isCSSSolidColor(
  value: CSSBackground | CSSUnknownArrayItem,
): value is CSSSolidColor {
  return value.type === 'solid'
}

interface URLFunctionBase {
  url: string
  enabled: boolean
}

export interface CSSURLFunction extends URLFunctionBase {
  type: 'url-function'
}

function cssURLFunction(value: string, enabled: boolean = true): CSSURLFunction {
  return {
    type: 'url-function',
    url: value,
    enabled,
  }
}

export function cssBackgroundLayerToCSSBackground(value: CSSBackgroundLayer): CSSBackground {
  switch (value.type) {
    case 'solid-background-layer': {
      return cssSolidColor(value.color, value.enabled)
    }
    case 'linear-gradient-background-layer': {
      return cssLinearGradient(value.stops, value.angle, value.enabled)
    }
    case 'conic-gradient-background-layer': {
      return cssConicGradient(value.stops, value.fromAngle, value.center, value.enabled)
    }
    case 'radial-gradient-background-layer': {
      return cssRadialGradient(value.stops, value.gradientSize, value.center, value.enabled)
    }
    case 'url-function-background-layer': {
      return cssURLFunction(value.url, value.enabled)
    }
    case 'unknown-array-item': {
      return value
    }
    default: {
      const _exhaustiveCheck: never = value
      throw new Error(`Unhandled background layer type ${value}`)
    }
  }
}

export function cssBackgroundToCSSBackgroundLayer(
  value: CSSBackground,
  bgSize: CSSBGSize,
): CSSBackgroundLayer {
  switch (value.type) {
    case 'solid': {
      return cssSolidBackgroundLayer(value)
    }
    case 'linear-gradient': {
      return cssLinearGradientBackgroundLayer(value, bgSize)
    }
    case 'conic-gradient': {
      return cssConicGradientBackgroundLayer(value, bgSize)
    }
    case 'radial-gradient': {
      return cssRadialGradientBackgroundLayer(value, bgSize)
    }
    case 'url-function': {
      return cssURLFunctionBackgroundLayer(value, bgSize)
    }
    case 'unknown-array-item': {
      return value
    }
    default: {
      const _exhaustiveCheck: never = value
      throw new Error(`Unhandled background layer type ${value}`)
    }
  }
}

export interface CSSKeyword<T extends string = string> {
  type: 'keyword'
  value: T
}

export function cssKeyword<T extends string>(value: T): CSSKeyword<T> {
  return { type: 'keyword', value }
}

export function isCSSKeyword(value: any): value is CSSKeyword {
  return typeof value === 'object' && value != null && value.type === 'keyword'
}

export function isCSSValidKeyword<T extends string>(
  value: unknown,
  validKeywords: ReadonlyArray<T>,
): value is CSSKeyword<T> {
  return (
    typeof value === 'object' &&
    value != null &&
    (value as any).type === 'keyword' &&
    validKeywords.includes((value as CSSKeyword).value as T)
  )
}

export function printCSSKeyword(keyword: CSSKeyword): string {
  return keyword.value
}

export interface ParsedCurlyBrace<T> {
  type: 'parsed-curly-brace'
  value: Array<T>
}

export function parsedCurlyBrace<T>(value: Array<T>): ParsedCurlyBrace<T> {
  return {
    type: 'parsed-curly-brace',
    value,
  }
}

export function isParsedCurlyBrace(value: unknown): value is ParsedCurlyBrace<any> {
  return typeof value === 'object' && value != null && (value as any).type === 'parsed-curly-brace'
}

export interface ParsedDoubleBar<T> {
  type: 'parsed-double-bar'
  value: Array<T>
}

export function parsedDoubleBar<T>(value: Array<T>): ParsedDoubleBar<T> {
  return {
    type: 'parsed-double-bar',
    value,
  }
}

export function isParsedDoubleBar(value: unknown): value is ParsedDoubleBar<any> {
  return typeof value === 'object' && value != null && (value as any).type === 'parsed-double-bar'
}

export interface HTMLImageElementMetadata {
  type: 'img-element-metadata'
  url: string
  enabled: boolean
}

function htmlImageElementMetadata(url: string, enabled = true): HTMLImageElementMetadata {
  return {
    type: 'img-element-metadata',
    url,
    enabled,
  }
}

function isHTMLImageElementMetadata(
  value: HTMLImageElementMetadata | CSSURLFunctionBackgroundLayer,
): value is CSSURLFunctionBackgroundLayer {
  return value.type === 'img-element-metadata'
}

const emptyHTMLImageElementMetadata: HTMLImageElementMetadata = {
  type: 'img-element-metadata',
  enabled: true,
  url: '',
}

export interface CSSUnknownFunctionParameters<T> {
  type: 'unknown-helper-function-parameters'
  value: JSExpressionValue<T>
}

export function isCSSUnknownFunctionParameters<T>(
  value: CSSUnknownFunctionParameters<T> | unknown,
): value is CSSUnknownFunctionParameters<T> {
  return (
    typeof value === 'object' &&
    value != null &&
    (value as any).type === 'unknown-helper-function-parameters'
  )
}

export function cssUnknownFunctionParameters<T>(value: T): CSSUnknownFunctionParameters<T> {
  return {
    type: 'unknown-helper-function-parameters',
    value: jsExpressionValue(value, emptyComments),
  }
}

export type CSSUnknownArrayItem = {
  type: 'unknown-array-item'
  value: string
  enabled: boolean
}

export function isCSSUnknownArrayItem(item: unknown): item is CSSUnknownArrayItem {
  return typeof item === 'object' && (item as any).type === 'unknown-array-item'
}

export function cssUnknownArrayItem(value: string, enabled: boolean = true): CSSUnknownArrayItem {
  return {
    type: 'unknown-array-item',
    value,
    enabled,
  }
}

export type EmptyInputValue = { type: 'EMPTY_INPUT_VALUE' }

export function isEmptyInputValue(value: unknown): value is EmptyInputValue {
  return typeof value === 'object' && value != null && (value as any).type === 'EMPTY_INPUT_VALUE'
}

export function emptyInputValue(): EmptyInputValue {
  return { type: 'EMPTY_INPUT_VALUE' }
}

export type UnknownInputValue = { type: 'UNKNOWN_INPUT'; value: string }

export function isUnknownInputValue(value: unknown): value is UnknownInputValue {
  return typeof value === 'object' && value != null && (value as any).type === 'UNKNOWN_INPUT'
}

export function unknownInputValue(value: string): UnknownInputValue {
  return { type: 'UNKNOWN_INPUT', value }
}

export type UnknownOrEmptyInput<T> = T | EmptyInputValue | UnknownInputValue

export const defaultGradientStops: Array<CSSGradientStop> = [
  {
    color: { ...blackHexCSSColor },
    position: {
      value: 0,
      unit: '%',
    },
  },
  {
    color: { ...whiteHexCSSColor },
    position: {
      value: 100,
      unit: '%',
    },
  },
]

export const defaultLinearGradientBackgroundLayer: CSSLinearGradientBackgroundLayer = {
  type: 'linear-gradient-background-layer',
  enabled: true,
  angle: {
    default: false,
    value: {
      value: 90,
      unit: 'deg',
    },
  },
  stops: [...defaultGradientStops],
  bgSize: { ...defaultBGSize },
}

export const defaultRadialGradientBackgroundLayer: CSSRadialGradientBackgroundLayer = {
  type: 'radial-gradient-background-layer',
  enabled: true,
  center: { ...defaultCSSRadialOrConicGradientCenter },
  gradientSize: { ...defaultCSSRadialGradientSize },
  stops: [...defaultGradientStops],
  bgSize: { ...defaultBGSize },
}

export const defaultConicGradientBackgroundLayer: CSSConicGradientBackgroundLayer = {
  type: 'conic-gradient-background-layer',
  enabled: true,
  fromAngle: {
    default: true,
    value: { ...zeroAngle },
  },
  center: { ...defaultCSSRadialOrConicGradientCenter },
  stops: [...defaultGradientStops],
  bgSize: { ...defaultBGSize },
}

export const defaultSolidBackgroundLayer: CSSSolidBackgroundLayer = {
  type: 'solid-background-layer',
  enabled: true,
  color: { ...defaultCSSColor },
}

export function isCSSBackground(value: CSSBackground | CSSColor): value is CSSBackground {
  return (
    value.type === 'linear-gradient' ||
    value.type === 'radial-gradient' ||
    value.type === 'conic-gradient' ||
    value.type === 'solid'
  )
}

export function orderStops(stops: Array<CSSGradientStop>): Array<CSSGradientStop> {
  return Utils.stripNulls(stops).sort((stopA, stopB) => stopA.position.value - stopB.position.value)
}

export function printStops(stops: Array<CSSGradientStop>): string {
  const orderedStops = orderStops(stops)
  const stopsStrings: Array<string> = []
  for (let i = 0; i < orderedStops.length; i++) {
    const stopColor = orderedStops[i].color
    const stopPosition = orderedStops[i].position
    stopsStrings.push(`${printColor(stopColor)} ${stopPosition.value}${stopPosition.unit}`)
  }
  return stopsStrings.join(', ')
}

export function printLinearGradientBackgroundLayer(
  gradient: CSSLinearGradient | CSSLinearGradientBackgroundLayer,
): string {
  const angle: string = gradient.angle.default
    ? ''
    : `${gradient.angle.value.value}${gradient.angle.value.unit}, `
  const stops: string = printStops(gradient.stops)
  return `linear-gradient(${angle}${stops})`
}

export function printRadialGradientBackgroundLayer(
  gradient: CSSRadialGradient | CSSRadialGradientBackgroundLayer,
): string {
  const s = gradient.gradientSize
  const shapeAndSize =
    gradient.gradientSize.width.default && gradient.gradientSize.height.default
      ? null
      : `${s.width.value.value}${s.width.value.unit} ${s.height.value.value}${s.height.value.unit}`
  const x = gradient.center.x
  const y = gradient.center.y
  const center =
    gradient.center.x.default && gradient.center.y.default
      ? null
      : // TODO support only printing x if y is default
        `at ${x.value.value}${x.value.unit} ${y.value.value}${y.value.unit}`
  const configuration = Utils.stripNulls([shapeAndSize, center]).join(' ')
  const stops = printStops(gradient.stops)
  return `radial-gradient(${configuration !== '' ? configuration + ', ' : ''}${stops})`
}

export function printConicGradientBackgroundLayer(
  gradient: CSSConicGradient | CSSConicGradientBackgroundLayer,
): string {
  const angle: string | null = gradient.fromAngle.default
    ? null
    : `from ${gradient.fromAngle.value.value}${gradient.fromAngle.value.unit}`
  const x = gradient.center.x
  const y = gradient.center.y
  const center =
    gradient.center.x.default && gradient.center.y.default
      ? null
      : // TODO support only printing x if y is default
        `at ${x.value.value}${x.value.unit} ${y.value.value}${y.value.unit}`
  const configuration: string = Utils.stripNulls([angle, center]).join(' ')
  const stops: string = printStops(gradient.stops)
  return `conic-gradient(${configuration !== '' ? configuration + ', ' : ''}${stops})`
}

function parseGradientStops(gradient: string): Either<string, Array<CSSGradientStop>> {
  const stops: Array<CSSGradientStop> = []
  RegExpLibrary.gradientColorStopValues.lastIndex = 0
  let stopMatches = RegExpLibrary.gradientColorStopValues.exec(gradient)
  while (stopMatches != null) {
    const parsedColor = parseColor(stopMatches[1], 'hex-hash-optional')
    const parsedPosition = parseCSSLengthPercent(stopMatches[27]) // TODO: make solids not have any position
    if (isRight(parsedColor) && isRight(parsedPosition)) {
      const stopResult: CSSGradientStop = {
        color: parsedColor.value,
        position: parsedPosition.value,
      }
      stops.push(stopResult)
      stopMatches = RegExpLibrary.gradientColorStopValues.exec(gradient)
    } else {
      return left(`Gradient stop parsing failed while parsing stop ${stopMatches[0]}`)
    }
  }
  if (stops.length < 2) {
    return left('Not enough stops in gradient')
  }
  return right(stops)
}

export function parseCSSURLFunction(match: string): Either<string, CSSURLFunction> {
  RegExpLibrary.urlFunction.lastIndex = 0
  const matchURLFunction = RegExpLibrary.urlFunction.exec(match)
  if (matchURLFunction != null) {
    const parsedURLStringContents =
      matchURLFunction[1] ?? matchURLFunction[2] ?? matchURLFunction[3]
    if (parsedURLStringContents != null) {
      return right(cssURLFunction(parsedURLStringContents))
    }
  }
  return left('No image url function found')
}

export function parseLinearGradient(
  match: string,
): Either<string, CSSLinearGradient | CSSSolidColor> {
  RegExpLibrary.linearGradient.lastIndex = 0
  const matchGradient = RegExpLibrary.linearGradient.exec(match)
  if (matchGradient != null) {
    const parsedAngle = parseCSSAngle(matchGradient[1])
    const parsedStops = parseGradientStops(matchGradient[2])
    if (isRight(parsedStops)) {
      const stops: Array<CSSGradientStop> = parsedStops.value
      if (
        stops.length === 2 &&
        isLeft(parsedAngle) &&
        printColor(stops[0].color) === printColor(stops[1].color)
      ) {
        return right({
          type: 'solid',
          enabled: true,
          color: stops[0].color,
        })
      }
      const finalValue: CSSLinearGradient = {
        type: 'linear-gradient',
        enabled: true,
        stops: stops,
        angle: isRight(parsedAngle)
          ? { default: false, value: parsedAngle.value }
          : { default: true, value: zeroAngle },
      }
      return right(finalValue)
    } else {
      return parsedStops
    }
  }
  return left('No linear-gradient found')
}

function parseGradientCenter(centerX: string, centerY: string): CSSRadialOrConicGradientCenter {
  const parsedCenterX = centerX != null ? parseCSSLengthPercent(centerX) : null
  const parsedCenterY = centerY != null ? parseCSSLengthPercent(centerY) : null
  if (
    parsedCenterX != null &&
    isRight(parsedCenterX) &&
    parsedCenterY != null &&
    isRight(parsedCenterY)
  ) {
    // TODO parse defaults properly
    return {
      x: cssDefault(parsedCenterX.value, false),
      y: cssDefault(parsedCenterY.value, false),
    }
  } else {
    return { ...defaultCSSRadialOrConicGradientCenter }
  }
}

export function parseRadialGradient(match: string): Either<string, CSSRadialGradient> {
  RegExpLibrary.radialGradient.lastIndex = 0
  const matchGradient = RegExpLibrary.radialGradient.exec(match)
  if (matchGradient != null) {
    const parsedEllipseWidth =
      matchGradient[1] != null ? parseCSSLengthPercent(matchGradient[1]) : null
    const parsedEllipseHeight =
      matchGradient[2] != null ? parseCSSLengthPercent(matchGradient[2]) : null
    let size: CSSRadialGradientSize =
      parsedEllipseWidth != null &&
      isRight(parsedEllipseWidth) &&
      parsedEllipseHeight != null &&
      isRight(parsedEllipseHeight)
        ? {
            width: cssDefault(parsedEllipseWidth.value),
            height: cssDefault(parsedEllipseHeight.value),
          }
        : { ...defaultCSSRadialGradientSize }

    const center = parseGradientCenter(matchGradient[3], matchGradient[4])

    const parsedStops = parseGradientStops(matchGradient[5])

    if (isRight(parsedStops)) {
      const stops: Array<CSSGradientStop> = parsedStops.value
      const finalValue: CSSRadialGradient = {
        type: 'radial-gradient',
        enabled: true,
        gradientSize: size,
        center,
        stops,
      }
      return right(finalValue)
    } else {
      return parsedStops
    }
  }
  return left('No radial-gradient found')
}

export function parseConicGradient(match: string): Either<string, CSSConicGradient> {
  RegExpLibrary.conicGradient.lastIndex = 0
  const matchGradient = RegExpLibrary.conicGradient.exec(match)
  if (matchGradient != null) {
    const parsedFromAngleDegree = parseCSSAngle(matchGradient[1])
    const parsedFromAnglePercent = parseCSSLengthPercent(matchGradient[1])
    let fromAngle: CSSDefault<CSSNumber>
    if (isRight(parsedFromAngleDegree)) {
      fromAngle = { default: false, value: parsedFromAngleDegree.value }
    } else if (isRight(parsedFromAnglePercent)) {
      fromAngle = { default: false, value: parsedFromAnglePercent.value }
    } else {
      fromAngle = {
        default: true,
        value: {
          value: 0,
          unit: '%',
        },
      }
    }
    const center = parseGradientCenter(matchGradient[2], matchGradient[3])
    const parsedStops = parseGradientStops(matchGradient[4])
    if (isRight(parsedStops)) {
      const stops: Array<CSSGradientStop> = parsedStops.value
      const finalValue: CSSConicGradient = {
        type: 'conic-gradient',
        enabled: true,
        center,
        stops,
        fromAngle,
      }
      return right(finalValue)
    } else {
      return parsedStops
    }
  }
  return left('No conic-gradient found')
}

const backgroundImageRegExp =
  /(\/\*)?(?:(?:((?:url|(?:linear|radial|conic)-gradient)\((?:\([^\)]*\)|[^\)\(]*)*\)),?)|((?:(?:repeating-linear|repeating-radial|repeating-conic)-gradient|image-set)\(.+\)))(\*\/)?/g

export function parseBackgroundImage(backgroundImage?: unknown): Either<string, CSSBackgrounds> {
  if (typeof backgroundImage === 'string') {
    let backgroundItems: Array<CSSBackground | CSSUnknownArrayItem> = []
    backgroundImageRegExp.lastIndex = 0
    let backgroundImageMatch = backgroundImageRegExp.exec(backgroundImage)
    while (backgroundImageMatch != null) {
      const cssFunctionMatch = backgroundImageMatch[2]
      const unsupportedMatch = backgroundImageMatch[3]
      const enabled = backgroundImageMatch[1] == null && backgroundImageMatch[4] == null
      if (cssFunctionMatch != null) {
        let parsedMatch: Either<string, CSSBackground> = left('No matching background image found')

        if (cssFunctionMatch.startsWith('linear')) {
          parsedMatch = parseLinearGradient(cssFunctionMatch)
        } else if (cssFunctionMatch.startsWith('radial')) {
          parsedMatch = parseRadialGradient(cssFunctionMatch)
        } else if (cssFunctionMatch.startsWith('conic')) {
          parsedMatch = parseConicGradient(cssFunctionMatch)
        } else if (cssFunctionMatch.startsWith('url')) {
          parsedMatch = parseCSSURLFunction(cssFunctionMatch)
        }
        if (isRight(parsedMatch)) {
          parsedMatch.value.enabled = enabled
          backgroundItems.push(parsedMatch.value)
        } else {
          backgroundItems.push({
            type: 'unknown-array-item',
            enabled,
            value: cssFunctionMatch,
          })
        }
      } else if (unsupportedMatch != null) {
        backgroundItems.push({
          type: 'unknown-array-item',
          enabled,
          value: unsupportedMatch,
        })
      }
      backgroundImageMatch = backgroundImageRegExp.exec(backgroundImage)
    }
    if (backgroundItems.length > 0) {
      return right(backgroundItems)
    }
  }
  return left('No valid backgroundImage found')
}

function printComma(layer: string, shouldCommaBePrinted: boolean): string {
  return `${layer}${shouldCommaBePrinted ? ',' : ''}`
}

function isLayerEnabled<T extends { enabled: boolean }>(layer: T): boolean {
  return layer.enabled
}

export function printBackgroundImage(
  cssBackgroundImages: CSSBackgrounds,
): JSExpressionValue<string> {
  const indexOfLastEnabledLayer = findLastIndex(isLayerEnabled, cssBackgroundImages)
  const backgroundImageStrings = cssBackgroundImages.map((backgroundImage, i) => {
    const enabled = backgroundImage.enabled
    const comma = indexOfLastEnabledLayer > i && enabled
    switch (backgroundImage.type) {
      case 'solid': {
        const color = printColor(backgroundImage.color)
        return printEnabled(
          printComma(`linear-gradient(${color} 0%, ${color} 100%)`, comma),
          enabled,
        )
      }
      case 'linear-gradient': {
        return printEnabled(
          printComma(`${printLinearGradientBackgroundLayer(backgroundImage)}`, comma),
          enabled,
        )
      }
      case 'radial-gradient': {
        return printEnabled(
          printComma(`${printRadialGradientBackgroundLayer(backgroundImage)}`, comma),
          enabled,
        )
      }
      case 'conic-gradient': {
        return printEnabled(
          printComma(`${printConicGradientBackgroundLayer(backgroundImage)}`, comma),
          enabled,
        )
      }
      case 'url-function': {
        return printEnabled(printComma(`url(${backgroundImage.url})`, comma), enabled)
      }
      case 'unknown-array-item': {
        return printEnabled(printComma(`${backgroundImage.value}`, comma), enabled)
      }
      default: {
        const _exhaustiveCheck: never = backgroundImage
        throw new Error(`Unhandled background layer type ${JSON.stringify(backgroundImage)}`)
      }
    }
  })
  return jsExpressionValue(backgroundImageStrings.join(' '), emptyComments)
}

export function printBackgroundSize(value: CSSBackgroundSize): JSExpressionValue<string> {
  const indexOfLastEnabledLayer = findLastIndex(isLayerEnabled, value)

  return jsExpressionValue(
    value
      .map((bgSize, i) => {
        const comma = indexOfLastEnabledLayer > i && bgSize.enabled
        switch (bgSize.size.value.type) {
          case 'keyword': {
            return printEnabled(
              printComma(printCSSKeyword(bgSize.size.value), comma),
              bgSize.enabled,
            )
          }
          case 'parsed-curly-brace': {
            return printEnabled(
              printComma(
                bgSize.size.value.value
                  .map((item) => {
                    if (isCSSNumber(item)) {
                      return printCSSNumber(item, null)
                    } else {
                      return printCSSKeyword(item)
                    }
                  })
                  .join(' '),
                comma,
              ),
              bgSize.enabled,
            )
          }
          default: {
            const _exhaustiveCheck: never = bgSize.size.value
            throw new Error(`CSSBGSize size ${bgSize.size} is not a keyword or parsed-curly-brace`)
          }
        }
      })
      .join(' '),
    emptyComments,
  )
}

export type CSSFontFamily = Array<string>

export type CSSFontWeight = number | 'bold' | 'normal'

/** Does not account for parsing the variable font feature for oblique angles */
export type CSSFontStyle = 'normal' | 'italic' | 'oblique'

export type CSSFontWeightAndStyle = {
  fontWeight: CSSFontWeight
  fontStyle: CSSFontStyle
}

export type CSSFontSize = CSSNumber

export type CSSTextAlign = 'left' | 'right' | 'center' | 'justify' | 'start' | 'end'

export type CSSDirection = 'ltr' | 'rtl'

export type CSSTextDecorationLine = string

export type CSSTextDecorationStyle = 'solid' | 'double' | 'dotted' | 'dashed' | 'wavy'

export type CSSTextDecorationColor = CSSColor

export interface CSSTextDecoration {
  textDecorationStyle: CSSTextDecorationStyle
  textDecorationLine: CSSTextDecorationLine
  textDecorationColor: CSSTextDecorationColor | undefined
}

export type CSSLetterSpacing = CSSNumber | 'normal'

export type CSSLineHeight = CSSNumber | 'normal'

export type CSSFontProperty =
  | CSSColor
  | CSSFontFamily
  | CSSFontWeight
  | CSSFontStyle
  | CSSFontWeightAndStyle
  | CSSFontSize
  | CSSTextAlign
  | CSSTextDecorationLine
  | CSSLetterSpacing
  | CSSLineHeight

export interface FontSettings {
  color: CSSColor
  fontFamily: CSSFontFamily
  fontWeightAndStyle: CSSFontWeightAndStyle
  fontSize: CSSFontSize
  textAlign: CSSTextAlign
  textDecorationLine: CSSTextDecorationLine
  letterSpacing: CSSLetterSpacing
  lineHeight: CSSLineHeight
}

export function fontSettings(
  color: CSSColor,
  fontFamily: CSSFontFamily,
  fontWeightAndStyle: CSSFontWeightAndStyle,
  fontSize: CSSFontSize,
  textAlign: CSSTextAlign,
  textDecorationLine: CSSTextDecorationLine,
  letterSpacing: CSSLetterSpacing,
  lineHeight: CSSLineHeight,
): FontSettings {
  return {
    color: color,
    fontFamily: fontFamily,
    fontWeightAndStyle: fontWeightAndStyle,
    fontSize: fontSize,
    textAlign: textAlign,
    textDecorationLine: textDecorationLine,
    letterSpacing: letterSpacing,
    lineHeight: lineHeight,
  }
}

export interface CSSTextShadow {
  enabled: boolean
  offsetX: CSSNumber
  offsetY: CSSNumber
  blurRadius: CSSDefault<CSSNumber>
  color: CSSColor
}

export type CSSTextShadows = ReadonlyArray<CSSTextShadow>

export const defaultTextShadow: CSSTextShadow = {
  enabled: true,
  offsetX: { ...cssPixelLengthZero },
  offsetY: { ...cssPixelLengthZero },
  blurRadius: cssDefault({ ...cssPixelLengthZero }),
  color: { ...defaultCSSColor },
}

export function parseTextShadow(textShadow: unknown): Either<string, CSSTextShadows> {
  if (typeof textShadow === 'string') {
    RegExpLibrary.textShadow.lastIndex = 0
    let matches = RegExpLibrary.textShadow.exec(textShadow)
    const textShadows: Array<CSSTextShadow> = []
    while (matches != null) {
      const enabled = matches[1] == null && matches[6] == null
      const parsedOffsetX = parseCSSLength(matches[2])
      const parsedOffsetY = parseCSSLength(matches[3])
      const parsedBlurRadius = parseCSSLength(matches[4])
      const blurRadius = cssDefault(
        isRight(parsedBlurRadius) ? parsedBlurRadius.value : { ...cssPixelLengthZero },
        !isRight(parsedBlurRadius),
      )
      const parsedColor = parseColor(matches[5], 'hex-hash-optional')
      if (isRight(parsedOffsetX) && isRight(parsedOffsetY) && isRight(parsedColor)) {
        const offsetX = parsedOffsetX.value
        const offsetY = parsedOffsetY.value
        const color = parsedColor.value
        textShadows.push({
          enabled,
          offsetX,
          offsetY,
          blurRadius,
          color,
        })
      }
      matches = RegExpLibrary.textShadow.exec(textShadow)
    }
    if (textShadows.length > 0) {
      return right(textShadows)
    }
  }
  return left('No text shadows found')
}

function printTextShadow(textShadows: CSSTextShadows): JSExpressionValue<string> {
  const indexOfLastEnabledLayer = findLastIndex(isLayerEnabled, textShadows)
  return jsExpressionValue(
    [...textShadows]
      .map((textShadow, i) => {
        const comma = indexOfLastEnabledLayer > i && textShadow.enabled
        const { offsetX, offsetY, blurRadius, color } = textShadow
        const parts = Utils.stripNulls([
          printCSSNumber(offsetX, null),
          printCSSNumber(offsetY, null),
          blurRadius == null ? null : printCSSNumber(blurRadius.value, null),
          printColor(color),
        ])
        return printEnabled(printComma(`${parts.join(' ')}`, comma), textShadow.enabled)
      })
      .join(' '),
    emptyComments,
  )
}

const quoteMarksRegexp = /['"]+/g

function parseFontFamily(fontFamily: unknown): Either<string, CSSFontFamily> {
  if (typeof fontFamily === 'string' && fontFamily.length > 0) {
    const trimmed = fontFamily.trim()
    const split = trimmed.split(',')
    const splitAndTrimmed = split.map((font) => font.trim().replace(quoteMarksRegexp, ''))
    return right(splitAndTrimmed)
  } else {
    return left('No valid fontFamily found')
  }
}

function printFontFamily(cssFontFamily: CSSFontFamily): JSExpressionValue<string> {
  return jsExpressionValue(fontFamilyArrayToCSSFontFamilyString(cssFontFamily), emptyComments)
}

function parseFontWeight(fontWeight: unknown): Either<string, CSSFontWeight> {
  if (typeof fontWeight === 'number') {
    if (fontWeight >= 1 && fontWeight <= 1000) {
      return right(fontWeight)
    } else {
      return left('fontWeight number is outside 1–1000 (inclusive) range')
    }
  } else if (fontWeight === 'normal' || fontWeight === 'bold') {
    return right(fontWeight)
  } else {
    return left('No valid fontWeight found')
  }
}

function printFontWeight(cssFontFamily: CSSFontWeight): JSExpressionValue<Property.FontWeight> {
  return jsExpressionValue(cssFontFamily, emptyComments)
}

const parseFontStyle = isOneOfTheseParser<CSSFontStyle>(['normal', 'italic'])

function printFontStyle(cssFontStyle: CSSFontStyle): JSExpressionValue<Property.FontStyle> {
  return jsExpressionValue(cssFontStyle, emptyComments)
}

const parseTextAlign = isOneOfTheseParser<CSSTextAlign>([
  'left',
  'right',
  'center',
  'justify',
  'start',
  'end',
])

export const parseDirection = isOneOfTheseParser<CSSDirection>(['ltr', 'rtl'])

function printTextAlign(cssTextAlign: CSSTextAlign): JSExpressionValue<Property.TextAlign> {
  return jsExpressionValue(cssTextAlign, emptyComments)
}

const parseTextDecorationLine = parseString

function printTextDecorationLine(
  cssTextDecorationLine: CSSTextDecorationLine,
): JSExpressionValue<Property.TextDecorationLine> {
  return jsExpressionValue(cssTextDecorationLine, emptyComments)
}

const parseTextDecorationStyle = isOneOfTheseParser<CSSTextDecorationStyle>([
  'solid',
  'double',
  'dotted',
  'dashed',
  'wavy',
])

function printTextDecorationStyle(
  cssTextDecorationStyle: CSSTextDecorationStyle,
): JSExpressionValue<Property.TextDecorationStyle> {
  return jsExpressionValue(cssTextDecorationStyle, emptyComments)
}

function parseLetterSpacing(letterSpacing: unknown): Either<string, CSSLetterSpacing> {
  if (letterSpacing === 'normal') {
    return right(letterSpacing)
  } else {
    return parseCSSLength(letterSpacing)
  }
}

function printLetterSpacing(
  cssLetterSpacing: CSSLetterSpacing,
): JSExpressionValue<Property.LetterSpacing<string | number>> {
  if (cssLetterSpacing === 'normal') {
    return jsExpressionValue(cssLetterSpacing, emptyComments)
  } else {
    return printCSSNumberAsAttributeValue(null)(cssLetterSpacing)
  }
}

function parseLineHeight(lineHeight: unknown): Either<string, CSSLineHeight> {
  if (lineHeight === 'normal') {
    return right(lineHeight)
  } else {
    return parseCSSLength(lineHeight)
  }
}

function printLineHeight(
  cssLineHeight: CSSLineHeight,
): JSExpressionValue<Property.LineHeight<string | number>> {
  if (cssLineHeight === 'normal') {
    return jsExpressionValue(cssLineHeight, emptyComments)
  } else {
    return printCSSNumberAsAttributeValue(null)(cssLineHeight)
  }
}

export function toggleSimple(attribute: ModifiableAttribute): ModifiableAttribute {
  if (modifiableAttributeIsAttributeFunctionCall(attribute)) {
    const result = jsxFunctionAttributeToRawValue(attribute)
    if (isRight(result) && result.value.functionName === disabledFunctionName) {
      const params = result.value.parameters
      if (params.length === 1) {
        const originalValueSimple = jsxSimpleAttributeToValue(params[0])
        if (isRight(originalValueSimple)) {
          return jsExpressionValue(originalValueSimple.value, emptyComments)
        } else {
          return params[0]
        }
      }
      return attribute
    } else {
      return attribute
    }
  } else {
    const simpleValue = jsxSimpleAttributeToValue(attribute)
    if (isRight(simpleValue)) {
      return jsExpressionFunctionCall(disabledFunctionName, [
        jsExpressionValue(simpleValue.value, emptyComments),
      ])
    } else if (isRegularJSXAttribute(attribute)) {
      return jsExpressionFunctionCall(disabledFunctionName, [attribute])
    } else {
      return attribute
    }
  }
}

const backgroundColorPathWithoutStyle = PP.create('backgroundColor')
const backgroundImagePathWithoutStyle = PP.create('backgroundImage')

const updateBackgroundImageLayersWithNewValues = (
  backgroundImageAttribute: ModifiableAttribute,
  newValueForAll: boolean | undefined,
  attributes: JSExpression,
): Either<string, JSExpression> => {
  let workingNewValueForAll = newValueForAll
  const simpleBackgroundImage = jsxSimpleAttributeToValue(backgroundImageAttribute)
  if (isRight(simpleBackgroundImage) && typeof simpleBackgroundImage.value === 'string') {
    const parsedBackgroundImage = parseBackgroundImage(simpleBackgroundImage.value)
    if (isRight(parsedBackgroundImage)) {
      const newBackgroundImage = [...parsedBackgroundImage.value].map((v) => {
        if (workingNewValueForAll == null) {
          workingNewValueForAll = !v.enabled
        }
        return {
          ...v,
          enabled: workingNewValueForAll,
        }
      })
      // set all backgroundImage layers to the new value
      return setJSXValueInAttributeAtPath(
        attributes,
        backgroundImagePathWithoutStyle,
        printBackgroundImage(newBackgroundImage),
      )
    }
  }
  return left('backgroundImage could not be parsed as valid backgroundImage string')
}

export function toggleBackgroundLayers(styleAttribute: JSExpression): JSExpression {
  let workingStyleProp: Either<string, JSExpression> = right(
    styleAttribute,
  ) as EitherRight<JSExpression>
  const backgroundColorResult = getJSExpressionAtPath(
    styleAttribute,
    backgroundColorPathWithoutStyle,
  )
  const backgroundImageResult = getJSExpressionAtPath(
    styleAttribute,
    backgroundImagePathWithoutStyle,
  )
  // If backgroundColor is set
  if (
    backgroundColorResult.remainingPath == null &&
    !modifiableAttributeIsAttributeNotFound(backgroundColorResult.attribute)
  ) {
    const simpleBackgroundColor = jsxSimpleAttributeToValue(backgroundColorResult.attribute)
    if (isRight(simpleBackgroundColor) && typeof simpleBackgroundColor.value === 'string') {
      const parsedBackgroundColor = parseBackgroundColor(simpleBackgroundColor.value)
      if (isRight(parsedBackgroundColor)) {
        // …use the opposite of its previous value for all new values…
        const newValueForAll = !parsedBackgroundColor.value.value.enabled
        let newBackgroundColor = { ...parsedBackgroundColor.value.value }
        newBackgroundColor.enabled = newValueForAll
        // …and set the backgroundColor value to it
        workingStyleProp = setJSXValueInAttributeAtPath(
          workingStyleProp.value,
          backgroundColorPathWithoutStyle,
          printBackgroundColor(cssDefault(newBackgroundColor, false)),
        )
        if (isRight(workingStyleProp)) {
          // If backgroundImage is also set…
          if (
            backgroundImageResult.remainingPath == null &&
            !modifiableAttributeIsAttributeNotFound(backgroundImageResult.attribute)
          ) {
            // …set all of its values to the new value
            workingStyleProp = updateBackgroundImageLayersWithNewValues(
              backgroundImageResult.attribute,
              newValueForAll,
              workingStyleProp.value,
            )
          }
          // If backgroundImage is not also set, just return the modified backgroundColor
        }
      }
    }
    // If backgroundColor is not set…
  } else {
    // …but backgroundImage is set…
    if (
      backgroundImageResult.remainingPath == null &&
      !modifiableAttributeIsAttributeNotFound(backgroundImageResult.attribute)
    ) {
      // …toggle backgroundImage
      workingStyleProp = updateBackgroundImageLayersWithNewValues(
        backgroundImageResult.attribute,
        undefined,
        workingStyleProp.value,
      )
      // If neither backgroundColor nor backgroundImage are set…
    } else {
      // …turn background color on
      workingStyleProp = setJSXValueInAttributeAtPath(
        workingStyleProp.value,
        backgroundColorPathWithoutStyle,
        printBackgroundColor(cssDefault(cssSolidColor({ ...defaultCSSColor }), true)),
      )
    }
  }
  return isRight(workingStyleProp) ? workingStyleProp.value : styleAttribute
}

export function toggleBorder(attribute: ModifiableAttribute): JSExpressionValue<string> {
  const simpleValue = jsxSimpleAttributeToValue(attribute)
  if (isRight(simpleValue) && typeof simpleValue.value === 'string') {
    const parsed = parseBorder(simpleValue.value)
    if (isRight(parsed)) {
      return printBorder(toggleBorderEnabled(null, parsed.value))
    }
  }
  return printBorder({ ...defaultCSSBorder })
}

export function toggleShadow(attribute: ModifiableAttribute): JSExpressionValue<string> {
  const simpleValue = jsxSimpleAttributeToValue(attribute)
  if (isRight(simpleValue) && typeof simpleValue.value === 'string') {
    const parsed = parseBoxShadow(simpleValue.value)
    if (isRight(parsed)) {
      return printBoxShadow(parsed.value.map((v) => toggleShadowEnabled(v)))
    }
  }
  return printBoxShadow([{ ...defaultBoxShadow }])
}

export function toggleStylePropPath(
  path: PropertyPath,
  toggleFn: (attribute: ModifiableAttribute) => ModifiableAttribute,
): (element: JSXElement) => JSXElement {
  return (element: JSXElement): JSXElement => {
    const attributeResult = getModifiableJSXAttributeAtPath(element.props, path)
    if (isRight(attributeResult)) {
      const attributeValue = attributeResult.value
      const updatedAttribute = toggleFn(attributeValue)
      const props: Either<string, JSXAttributes> =
        modifiableAttributeIsAttributeNotFound(updatedAttribute) ||
        modifiableAttributeIsPartOfAttributeValue(updatedAttribute)
          ? left(`Unable to set value of type ${updatedAttribute.type}`)
          : setJSXValueAtPath(element.props, path, updatedAttribute)
      if (isLeft(props)) {
        return element
      } else {
        return {
          ...element,
          props: props.value,
        }
      }
    }
    return element
  }
}

export function toggleStylePropPaths(
  toggleFn: (attribute: JSExpression) => JSExpression,
): (element: JSXElement) => JSXElement {
  return (element: JSXElement): JSXElement => {
    const styleProp = getJSXAttributesAtPath(element.props, PP.create('style'))
    const attribute = styleProp.attribute
    if (styleProp.remainingPath == null && isRegularJSXAttribute(attribute)) {
      const newProps = setJSXValueAtPath(element.props, PP.create('style'), toggleFn(attribute))
      if (isRight(newProps)) {
        return { ...element, props: newProps.value }
      }
    }
    // TODO: toast if failure?
    return element
  }
}

function printCSSNumberAsAttributeValue(
  defaultUnitToSkip: string | null,
): (value: CSSNumber) => JSExpressionValue<string | number> {
  return (value: CSSNumber) =>
    jsExpressionValue(printCSSNumber(value, defaultUnitToSkip), emptyComments)
}

function printCSSNumberOrUndefinedAsAttributeValue(
  defaultUnitToSkip: string | null,
): (value: CSSNumber | undefined) => JSExpressionValue<string | number | undefined> {
  return (value: CSSNumber | undefined) => {
    return value != null
      ? printCSSNumberAsAttributeValue(defaultUnitToSkip)(value)
      : jsExpressionValue(undefined, emptyComments)
  }
}

const printCSSNumberUnitlessOrUndefinedAsAttributeValue = (
  value: CSSNumber | undefined,
): JSExpressionValue<string | number | undefined> => {
  return value != null
    ? jsExpressionValue(fixNumber(value.value), emptyComments)
    : jsExpressionValue(undefined, emptyComments)
}

function parseString(value: unknown): Either<string, string> {
  return typeof value === 'string' ? right(value) : left(`${value} is not a string`)
}

function printStringAsAttributeValue(value: string): JSExpressionValue<string> {
  return jsExpressionValue(value, emptyComments)
}

type CSSMixBlendMode = 'normal' | 'multiply' | 'screen' | 'darken'

const parseMixBlendMode = isOneOfTheseParser<CSSMixBlendMode>([
  'normal',
  'multiply',
  'screen',
  'darken',
])

function printMixBlendMode(blendMode: CSSMixBlendMode): JSExpressionValue<string> {
  return jsExpressionValue(blendMode, emptyComments)
}

type CSSObjectFit = 'fill' | 'contain' | 'cover' | 'none' | 'scale-down'
const objectFitTypes: Array<CSSObjectFit> = ['fill', 'contain', 'cover', 'none', 'scale-down']

const parseCSSObjectFit = isOneOfTheseParser<CSSObjectFit>([
  'fill',
  'contain',
  'cover',
  'none',
  'scale-down',
])

type ImageURL = string

function printCSSObjectFit(value: CSSObjectFit): JSExpressionValue<string> {
  return jsExpressionValue(value, emptyComments)
}

function parseFramePin(
  simpleValue: unknown | null,
  _: ModifiableAttribute | null,
): Either<string, CSSNumber> {
  return parseCSSNumber(simpleValue, 'LengthPercent')
}

function isOneOfTheseParser<T extends PrimitiveType>(values: Array<T>): Parser<T> {
  return (simpleValue: unknown | null, _: ModifiableAttribute | null) => {
    if (values.includes(simpleValue as T)) {
      return right(simpleValue as T)
    } else {
      return left('Invalid value.')
    }
  }
}

const layoutSystemParser: Parser<LayoutSystem> = isOneOfTheseParser([
  LayoutSystem.PinSystem,
  LayoutSystem.Group,
])

const flexWrapParser: Parser<FlexWrap> = isOneOfTheseParser([
  FlexWrap.NoWrap,
  FlexWrap.Wrap,
  FlexWrap.WrapReverse,
])

export type Direction = 'horizontal' | 'vertical'
export type ForwardOrReverse = 'forward' | 'reverse'

export interface SimpleFlexDirection {
  direction: Direction
  forwardOrReverse: ForwardOrReverse
}

export type FlexDirection = 'row' | 'row-reverse' | 'column' | 'column-reverse'
export const AllFlexDirections: Array<FlexDirection> = [
  'row',
  'row-reverse',
  'column',
  'column-reverse',
]

export const parseFlexDirection: Parser<FlexDirection> = isOneOfTheseParser(AllFlexDirections)

const flexAlignmentsParser: Parser<FlexAlignment> = isOneOfTheseParser([
  FlexAlignment.Auto,
  FlexAlignment.FlexStart,
  FlexAlignment.Center,
  FlexAlignment.FlexEnd,
  FlexAlignment.Stretch,
])

const flexJustifyContentParser: Parser<FlexJustifyContent> = isOneOfTheseParser([
  FlexJustifyContent.FlexStart,
  FlexJustifyContent.Center,
  FlexJustifyContent.FlexEnd,
  FlexJustifyContent.SpaceAround,
  FlexJustifyContent.SpaceBetween,
  FlexJustifyContent.SpaceEvenly,
])

export type CSSPosition = '-webkit-sticky' | 'absolute' | 'fixed' | 'relative' | 'static' | 'sticky'
export const positionValues: Array<CSSPosition> = [
  '-webkit-sticky',
  'absolute',
  'fixed',
  'relative',
  'static',
  'sticky',
]
const flexPositionParser: Parser<CSSPosition> = isOneOfTheseParser(positionValues)

function isNumberParser(simpleValue: unknown): Either<string, number> {
  if (typeof simpleValue === 'number') {
    return right(simpleValue)
  } else {
    return left('Not a valid number.')
  }
}

type DOMEventHandlerMetadata = JSExpression

export function parseDOMEventHandlerMetadata(
  _: unknown,
  attribute: ModifiableAttribute | null,
): Either<string, DOMEventHandlerMetadata> {
  if (attribute == null) {
    return left('found no event handler')
  }
  if (attribute.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
    return right(attribute)
  } else {
    return left('Event handler value is not ATTRIBUTE_OTHER_JAVASCRIPT')
  }
}

export function printDOMEventHandlerMetadata(value: JSExpression): JSExpression {
  return value
}

export interface ParsedCSSProperties {
  backgroundColor: CSSDefault<CSSSolidColor>
  backgroundImage: CSSBackgrounds
  backgroundSize: CSSBackgroundSize
  border: CSSBorder
  borderRadius: CSSBorderRadius
  borderTopLeftRadius: CSSNumber
  borderTopRightRadius: CSSNumber
  borderBottomLeftRadius: CSSNumber
  borderBottomRightRadius: CSSNumber
  boxShadow: CSSBoxShadows
  color: CSSColor
  fontFamily: CSSFontFamily
  fontSize: CSSFontSize
  fontStyle: CSSFontStyle
  fontWeight: CSSFontWeight
  letterSpacing: CSSLetterSpacing
  lineHeight: CSSLineHeight
  mixBlendMode: CSSMixBlendMode
  opacity: CSSNumber
  overflow: CSSOverflow
  textAlign: CSSTextAlign
  textDecorationColor: CSSTextDecorationColor | undefined
  textDecorationLine: CSSTextDecorationLine
  textDecorationStyle: CSSTextDecorationStyle
  textShadow: CSSTextShadows
  transform: CSSTransforms
  transformOrigin: CSSTransformOrigin

  objectFit: CSSObjectFit

  padding: CSSPadding
  paddingTop: CSSNumber
  paddingRight: CSSNumber
  paddingBottom: CSSNumber
  paddingLeft: CSSNumber
  margin: CSSMargin
  marginTop: CSSNumber
  marginRight: CSSNumber
  marginBottom: CSSNumber
  marginLeft: CSSNumber
  flexWrap: FlexWrap
  flexDirection: FlexDirection
  alignItems: FlexAlignment
  alignContent: FlexAlignment
  justifyContent: FlexJustifyContent
  alignSelf: FlexAlignment
  position: CSSPosition
  left: CSSNumber | undefined
  top: CSSNumber | undefined
  right: CSSNumber | undefined
  bottom: CSSNumber | undefined
  minWidth: CSSNumber | undefined
  maxWidth: CSSNumber | undefined
  minHeight: CSSNumber | undefined
  maxHeight: CSSNumber | undefined
  flex: CSSFlex
  flexGrow: number
  flexShrink: number
  display: string
  width: CSSNumber | undefined
  height: CSSNumber | undefined
  flexBasis: CSSNumber | undefined
  gap: CSSNumber
  zIndex: CSSNumber | undefined
}

export type ParsedCSSPropertiesKeys = keyof ParsedCSSProperties
export type ParsedCSSPropertiesKeysNoLayout = keyof Omit<ParsedCSSProperties, StyleLayoutProp>

export function fallbackOnEmptyInputValueToCSSEmptyValue<T>(
  emptyValue: T,
  value: T | EmptyInputValue,
): T {
  return isEmptyInputValue(value) ? emptyValue : value
}

export function fallbackOnEmptyInputValueToCSSDefaultEmptyValue<T>(
  emptyDefaultValue: CSSDefault<T>,
  value: T | EmptyInputValue,
): CSSDefault<T> {
  return isEmptyInputValue(value) ? emptyDefaultValue : cssDefault(value, false)
}

export const cssEmptyValues: ParsedCSSProperties = {
  backgroundColor: cssDefault({ ...emptyBackgroundColor }),
  backgroundImage: [],
  backgroundSize: [],
  border: { ...emptyCSSBorder },
  borderRadius: {
    type: 'LEFT',
    value: {
      value: 0,
      unit: 'px',
    },
  },
  borderTopLeftRadius: {
    value: 0,
    unit: 'px',
  },
  borderTopRightRadius: {
    value: 0,
    unit: 'px',
  },
  borderBottomLeftRadius: {
    value: 0,
    unit: 'px',
  },
  borderBottomRightRadius: {
    value: 0,
    unit: 'px',
  },
  boxShadow: [],
  color: {
    type: 'Hex',
    hex: '#444444',
  },
  fontFamily: ['-apple-system', 'BlinkMacSystemFont', 'sans-serif'],
  fontSize: {
    value: 16,
    unit: 'px',
  },
  fontStyle: 'normal',
  fontWeight: 400,
  letterSpacing: {
    value: 0,
    unit: 'px',
  },
  lineHeight: {
    value: 1.25,
    unit: null,
  },
  mixBlendMode: 'normal',
  opacity: cssNumber(1),
  overflow: true,
  textAlign: 'left',
  textDecorationColor: undefined,
  textDecorationLine: 'none',
  textDecorationStyle: 'solid',
  textShadow: [],
  transform: [],
  transformOrigin: {
    x: CSSTransformOriginStringValueX.Center,
    y: CSSTransformOriginStringValueY.Center,
  },

  objectFit: 'fill',

  flexWrap: FlexWrap.NoWrap,
  flexDirection: 'row',
  alignItems: FlexAlignment.FlexStart,
  alignContent: FlexAlignment.FlexStart,
  justifyContent: FlexJustifyContent.FlexStart,
  padding: {
    paddingTop: {
      value: 0,
      unit: null,
    },
    paddingRight: {
      value: 0,
      unit: null,
    },
    paddingBottom: {
      value: 0,
      unit: null,
    },
    paddingLeft: {
      value: 0,
      unit: null,
    },
  },
  paddingTop: {
    value: 0,
    unit: null,
  },
  paddingRight: {
    value: 0,
    unit: null,
  },
  paddingBottom: {
    value: 0,
    unit: null,
  },
  paddingLeft: {
    value: 0,
    unit: null,
  },
  alignSelf: FlexAlignment.Auto,
  position: 'relative',
  left: {
    value: 0,
    unit: null,
  },
  top: {
    value: 0,
    unit: null,
  },
  right: {
    value: 0,
    unit: null,
  },
  bottom: {
    value: 0,
    unit: null,
  },
  minWidth: undefined,
  maxWidth: undefined,
  minHeight: undefined,
  maxHeight: undefined,
  margin: {
    marginTop: {
      value: 0,
      unit: null,
    },
    marginRight: {
      value: 0,
      unit: null,
    },
    marginBottom: {
      value: 0,
      unit: null,
    },
    marginLeft: {
      value: 0,
      unit: null,
    },
  },
  marginTop: {
    value: 0,
    unit: null,
  },
  marginRight: {
    value: 0,
    unit: null,
  },
  marginBottom: {
    value: 0,
    unit: null,
  },
  marginLeft: {
    value: 0,
    unit: null,
  },
  flex: {
    flexGrow: 0,
    flexShrink: 1,
    flexBasis: {
      value: 0,
      unit: null,
    },
  },
  flexGrow: 0,
  flexShrink: 1,
  display: 'block',
  gap: {
    value: 0,
    unit: null,
  },
  width: {
    value: 0,
    unit: null,
  },
  height: {
    value: 0,
    unit: null,
  },
  flexBasis: {
    value: 0,
    unit: null,
  },
  zIndex: undefined,
}

type CSSParsers = {
  [key in keyof ParsedCSSProperties]: Parser<ParsedCSSProperties[key]>
}

export const cssParsers: CSSParsers = {
  backgroundColor: parseBackgroundColor,
  backgroundImage: parseBackgroundImage,
  backgroundSize: parseBackgroundSize,
  border: parseBorder,
  borderRadius: parseBorderRadius,
  borderTopLeftRadius: parseCSSLengthPercent,
  borderTopRightRadius: parseCSSLengthPercent,
  borderBottomLeftRadius: parseCSSLengthPercent,
  borderBottomRightRadius: parseCSSLengthPercent,
  boxShadow: parseBoxShadow,
  color: parseColorHexHashOptional,
  fontFamily: parseFontFamily,
  fontSize: parseCSSLengthPercent,
  fontStyle: parseFontStyle,
  fontWeight: parseFontWeight,
  letterSpacing: parseLetterSpacing,
  lineHeight: parseLineHeight,
  mixBlendMode: parseMixBlendMode,
  opacity: parseCSSUnitlessPercent,
  overflow: parseOverflow,
  textAlign: parseTextAlign,
  textDecorationColor: parseColorHexHashOptional,
  textDecorationLine: parseTextDecorationLine,
  textDecorationStyle: parseTextDecorationStyle,
  textShadow: parseTextShadow,
  transform: parseTransform,
  transformOrigin: parseTransformOrigin,

  objectFit: parseCSSObjectFit,

  flexWrap: flexWrapParser,
  flexDirection: parseFlexDirection,
  alignItems: flexAlignmentsParser,
  alignContent: flexAlignmentsParser,
  justifyContent: flexJustifyContentParser,
  padding: parsePadding,
  paddingTop: parseCSSLengthPercent,
  paddingRight: parseCSSLengthPercent,
  paddingBottom: parseCSSLengthPercent,
  paddingLeft: parseCSSLengthPercent,

  alignSelf: flexAlignmentsParser,
  position: flexPositionParser,
  left: parseCSSLengthPercent,
  top: parseCSSLengthPercent,
  right: parseCSSLengthPercent,
  bottom: parseCSSLengthPercent,
  minWidth: parseCSSLengthPercent,
  maxWidth: parseCSSLengthPercentNone,
  minHeight: parseCSSLengthPercent,
  maxHeight: parseCSSLengthPercentNone,
  margin: parseMargin,
  marginTop: parseCSSLengthPercent,
  marginRight: parseCSSLengthPercent,
  marginBottom: parseCSSLengthPercent,
  marginLeft: parseCSSLengthPercent,
  flex: parseFlex,
  flexGrow: parseCSSUnitlessAsNumber,
  flexShrink: parseCSSUnitlessAsNumber,
  display: parseDisplay,
  gap: parseCSSLengthPercent,
  width: parseCSSLengthPercent,
  height: parseCSSLengthPercent,
  flexBasis: parseCSSLengthPercent,
  zIndex: parseCSSUnitless,
}

type CSSPrinters = {
  [key in keyof ParsedCSSProperties]: Printer<ParsedCSSProperties[key]>
}

const jsxAttributeValueWithNoComments = (value: unknown) => jsExpressionValue(value, emptyComments)

const cssPrinters: CSSPrinters = {
  backgroundColor: printBackgroundColor,
  backgroundImage: printBackgroundImage,
  backgroundSize: printBackgroundSize,
  mixBlendMode: printMixBlendMode,
  border: printBorder,
  borderRadius: printBorderRadius,
  borderTopLeftRadius: printCSSNumberAsAttributeValue('px'),
  borderTopRightRadius: printCSSNumberAsAttributeValue('px'),
  borderBottomLeftRadius: printCSSNumberAsAttributeValue('px'),
  borderBottomRightRadius: printCSSNumberAsAttributeValue('px'),
  boxShadow: printBoxShadow,
  color: printColorToJsx,
  fontFamily: printFontFamily,
  fontSize: printCSSNumberAsAttributeValue(null),
  fontStyle: printFontStyle,
  fontWeight: printFontWeight,
  letterSpacing: printLetterSpacing,
  lineHeight: printLineHeight,
  opacity: printCSSNumberAsAttributeValue(null),
  overflow: printOverflow,
  textAlign: printTextAlign,
  textDecorationColor: printColorToJsx,
  textDecorationLine: printTextDecorationLine,
  textDecorationStyle: printTextDecorationStyle,
  textShadow: printTextShadow,
  transform: printTransform,
  transformOrigin: printTransformOrigin,

  objectFit: printCSSObjectFit,

  flexWrap: jsxAttributeValueWithNoComments,
  flexDirection: jsxAttributeValueWithNoComments,
  alignItems: jsxAttributeValueWithNoComments,
  alignContent: jsxAttributeValueWithNoComments,
  justifyContent: jsxAttributeValueWithNoComments,
  padding: printPaddingAsAttributeValue,
  paddingTop: printCSSNumberAsAttributeValue('px'),
  paddingRight: printCSSNumberAsAttributeValue('px'),
  paddingBottom: printCSSNumberAsAttributeValue('px'),
  paddingLeft: printCSSNumberAsAttributeValue('px'),

  alignSelf: jsxAttributeValueWithNoComments,
  position: jsxAttributeValueWithNoComments,
  left: printCSSNumberOrUndefinedAsAttributeValue('px'),
  top: printCSSNumberOrUndefinedAsAttributeValue('px'),
  right: printCSSNumberOrUndefinedAsAttributeValue('px'),
  bottom: printCSSNumberOrUndefinedAsAttributeValue('px'),
  minWidth: printCSSNumberOrUndefinedAsAttributeValue('px'),
  maxWidth: printCSSNumberOrUndefinedAsAttributeValue('px'),
  minHeight: printCSSNumberOrUndefinedAsAttributeValue('px'),
  maxHeight: printCSSNumberOrUndefinedAsAttributeValue('px'),
  margin: printMarginAsAttributeValue,
  marginTop: printCSSNumberAsAttributeValue('px'),
  marginRight: printCSSNumberAsAttributeValue('px'),
  marginBottom: printCSSNumberAsAttributeValue('px'),
  marginLeft: printCSSNumberAsAttributeValue('px'),
  flex: printFlexAsAttributeValue,
  flexGrow: jsxAttributeValueWithNoComments,
  flexShrink: jsxAttributeValueWithNoComments,
  display: printStringAsAttributeValue,
  width: printCSSNumberOrUndefinedAsAttributeValue('px'),
  height: printCSSNumberOrUndefinedAsAttributeValue('px'),
  flexBasis: printCSSNumberOrUndefinedAsAttributeValue('px'),
  gap: printCSSNumberAsAttributeValue('px'),
  zIndex: printCSSNumberUnitlessOrUndefinedAsAttributeValue,
}

export interface UtopianElementProperties {
  className: string
}

interface DOMIMGAttributeProperties {
  alt: string
  src: ImageURL
}

/** copied from @types/react/index.d.ts DOMAttributes */
export const DOMEventHandlerNames = [
  'onCopy',
  'onCopyCapture',
  'onCut',
  'onCutCapture',
  'onPaste',
  'onPasteCapture',
  'onCompositionEnd',
  'onCompositionEndCapture',
  'onCompositionStart',
  'onCompositionStartCapture',
  'onCompositionUpdate',
  'onCompositionUpdateCapture',
  'onFocus',
  'onFocusCapture',
  'onBlur',
  'onBlurCapture',
  'onChange',
  'onChangeCapture',
  'onBeforeInput',
  'onBeforeInputCapture',
  'onInput',
  'onInputCapture',
  'onReset',
  'onResetCapture',
  'onSubmit',
  'onSubmitCapture',
  'onInvalid',
  'onInvalidCapture',
  'onLoad',
  'onLoadCapture',
  'onError',
  'onErrorCapture',
  'onKeyDown',
  'onKeyDownCapture',
  'onKeyPress',
  'onKeyPressCapture',
  'onKeyUp',
  'onKeyUpCapture',
  'onAbort',
  'onAbortCapture',
  'onCanPlay',
  'onCanPlayCapture',
  'onCanPlayThrough',
  'onCanPlayThroughCapture',
  'onDurationChange',
  'onDurationChangeCapture',
  'onEmptied',
  'onEmptiedCapture',
  'onEncrypted',
  'onEncryptedCapture',
  'onEnded',
  'onEndedCapture',
  'onLoadedData',
  'onLoadedDataCapture',
  'onLoadedMetadata',
  'onLoadedMetadataCapture',
  'onLoadStart',
  'onLoadStartCapture',
  'onPause',
  'onPauseCapture',
  'onPlay',
  'onPlayCapture',
  'onPlaying',
  'onPlayingCapture',
  'onProgress',
  'onProgressCapture',
  'onRateChange',
  'onRateChangeCapture',
  'onSeeked',
  'onSeekedCapture',
  'onSeeking',
  'onSeekingCapture',
  'onStalled',
  'onStalledCapture',
  'onSuspend',
  'onSuspendCapture',
  'onTimeUpdate',
  'onTimeUpdateCapture',
  'onVolumeChange',
  'onVolumeChangeCapture',
  'onWaiting',
  'onWaitingCapture',
  'onAuxClick',
  'onAuxClickCapture',
  'onClick',
  'onClickCapture',
  'onContextMenu',
  'onContextMenuCapture',
  'onDoubleClick',
  'onDoubleClickCapture',
  'onDrag',
  'onDragCapture',
  'onDragEnd',
  'onDragEndCapture',
  'onDragEnter',
  'onDragEnterCapture',
  'onDragExit',
  'onDragExitCapture',
  'onDragLeave',
  'onDragLeaveCapture',
  'onDragOver',
  'onDragOverCapture',
  'onDragStart',
  'onDragStartCapture',
  'onDrop',
  'onDropCapture',
  'onMouseDown',
  'onMouseDownCapture',
  'onMouseEnter',
  'onMouseLeave',
  'onMouseMove',
  'onMouseMoveCapture',
  'onMouseOut',
  'onMouseOutCapture',
  'onMouseOver',
  'onMouseOverCapture',
  'onMouseUp',
  'onMouseUpCapture',
  'onSelect',
  'onSelectCapture',
  'onTouchCancel',
  'onTouchCancelCapture',
  'onTouchEnd',
  'onTouchEndCapture',
  'onTouchMove',
  'onTouchMoveCapture',
  'onTouchStart',
  'onTouchStartCapture',
  'onPointerDown',
  'onPointerDownCapture',
  'onPointerMove',
  'onPointerMoveCapture',
  'onPointerUp',
  'onPointerUpCapture',
  'onPointerCancel',
  'onPointerCancelCapture',
  'onPointerEnter',
  'onPointerEnterCapture',
  'onPointerLeave',
  'onPointerLeaveCapture',
  'onPointerOver',
  'onPointerOverCapture',
  'onPointerOut',
  'onPointerOutCapture',
  'onGotPointerCapture',
  'onGotPointerCaptureCapture',
  'onLostPointerCapture',
  'onLostPointerCaptureCapture',
  'onScroll',
  'onScrollCapture',
  'onWheel',
  'onWheelCapture',
  'onAnimationStart',
  'onAnimationStartCapture',
  'onAnimationEnd',
  'onAnimationEndCapture',
  'onAnimationIteration',
  'onAnimationIterationCapture',
  'onTransitionEnd',
  'onTransitionEndCapture',
] as const
export type DOMEventHandler = NonNullable<(typeof DOMEventHandlerNames)[number]>

type DOMEventAttributeProperties = {
  [key in DOMEventHandler]: DOMEventHandlerMetadata
}

export interface ParsedElementProperties
  extends UtopianElementProperties,
    DOMIMGAttributeProperties,
    DOMEventAttributeProperties {}

export type ParsedElementPropertiesKeys = keyof ParsedElementProperties

export const DOMEventHandlerEmptyValues = DOMEventHandlerNames.reduce((current, item) => {
  current[item] = jsExpressionValue(undefined, emptyComments)
  return current
}, {} as DOMEventAttributeProperties)

const elementPropertiesEmptyValuesExcludingEvents: UtopianElementProperties &
  DOMIMGAttributeProperties = {
  alt: '',
  src: '/',
  className: '',
}

const elementPropertiesEmptyValues: ParsedElementProperties = {
  alt: '',
  src: '/',
  ...DOMEventHandlerEmptyValues,
  className: '',
}

type MetadataParsers = {
  [key in keyof ParsedElementProperties]: Parser<ParsedElementProperties[key]>
}

const DOMEventHandlerParsers = DOMEventHandlerNames.reduce((current, item) => {
  current[item] = parseDOMEventHandlerMetadata
  return current
}, {} as { [key in DOMEventHandler]: Parser<DOMEventHandlerMetadata> })

const elementPropertiesParsers: MetadataParsers = {
  alt: parseString,
  src: parseString,
  ...DOMEventHandlerParsers,
  className: parseString,
}

type MetadataPrinters = {
  [key in keyof ParsedElementProperties]: Printer<ParsedElementProperties[key]>
}

const DOMEventHandlerPrinters = DOMEventHandlerNames.reduce((current, item) => {
  current[item] = printDOMEventHandlerMetadata
  return current
}, {} as { [key in DOMEventHandler]: Printer<DOMEventHandlerMetadata> })

const elementPropertiesPrinters: MetadataPrinters = {
  alt: printStringAsAttributeValue,
  src: printStringAsAttributeValue,
  ...DOMEventHandlerPrinters,
  className: printStringAsAttributeValue,
}

interface ParsedLayoutProperties {
  pinLeft: CSSNumber | undefined
  pinRight: CSSNumber | undefined
  centerX: CSSNumber | undefined
  width: CSSNumber | undefined
  pinTop: CSSNumber | undefined
  pinBottom: CSSNumber | undefined
  centerY: CSSNumber | undefined
  height: CSSNumber | undefined
  gapMain: CSSNumber
  flexBasis: CSSNumber | undefined
}

export const layoutEmptyValues: ParsedLayoutProperties = {
  pinLeft: undefined,
  pinRight: undefined,
  centerX: undefined,
  width: undefined,
  pinTop: undefined,
  pinBottom: undefined,
  centerY: undefined,
  height: undefined,
  gapMain: { value: 0, unit: null },
  flexBasis: undefined,
}

type LayoutParsers = {
  [key in keyof ParsedLayoutProperties]: Parser<ParsedLayoutProperties[key]>
}

const layoutParsers: LayoutParsers = {
  pinLeft: parseFramePin,
  pinRight: parseFramePin,
  centerX: parseFramePin,
  width: parseFramePin,
  pinTop: parseFramePin,
  pinBottom: parseFramePin,
  centerY: parseFramePin,
  height: parseFramePin,
  gapMain: parseCSSLengthPercent,
  flexBasis: parseFramePin,
}

type LayoutPrinters = {
  [key in keyof ParsedLayoutProperties]: Printer<ParsedLayoutProperties[key]>
}

const layoutPrinters: LayoutPrinters = {
  pinLeft: jsxAttributeValueWithNoComments,
  pinRight: jsxAttributeValueWithNoComments,
  centerX: jsxAttributeValueWithNoComments,
  width: jsxAttributeValueWithNoComments,
  pinTop: jsxAttributeValueWithNoComments,
  pinBottom: jsxAttributeValueWithNoComments,
  centerY: jsxAttributeValueWithNoComments,
  height: jsxAttributeValueWithNoComments,
  gapMain: jsxAttributeValueWithNoComments,
  flexBasis: jsxAttributeValueWithNoComments,
}

const layoutEmptyValuesNew: LayoutPropertyTypes = {
  width: undefined,
  height: undefined,

  gap: { value: 0, unit: null },
  flexBasis: undefined,

  left: undefined,
  top: undefined,
  right: undefined,
  bottom: undefined,
}

type LayoutParsersNew = {
  [key in keyof LayoutPropertyTypes]: Parser<LayoutPropertyTypes[key]>
}

const layoutParsersNew: LayoutParsersNew = {
  width: parseFramePin,
  height: parseFramePin,

  gap: parseCSSLengthPercent,
  flexBasis: parseFramePin,

  left: parseFramePin,
  top: parseFramePin,
  right: parseFramePin,
  bottom: parseFramePin,
}

type LayoutPrintersNew = {
  [key in keyof LayoutPropertyTypes]: Printer<LayoutPropertyTypes[key]>
}

const layoutPrintersNew: LayoutPrintersNew = {
  width: printCSSNumberOrUndefinedAsAttributeValue('px'),
  height: printCSSNumberOrUndefinedAsAttributeValue('px'),

  gap: printCSSNumberOrUndefinedAsAttributeValue('px'),
  flexBasis: printCSSNumberOrUndefinedAsAttributeValue('px'),

  left: printCSSNumberOrUndefinedAsAttributeValue('px'),
  top: printCSSNumberOrUndefinedAsAttributeValue('px'),
  right: printCSSNumberOrUndefinedAsAttributeValue('px'),
  bottom: printCSSNumberOrUndefinedAsAttributeValue('px'),
}

export interface ParsedProperties
  extends ParsedElementProperties,
    ParsedCSSProperties,
    ParsedLayoutProperties,
    LayoutPropertyTypes {}

export type ParsedPropertiesKeys = keyof ParsedProperties

export type ParsedPropertiesValues = ParsedProperties[ParsedPropertiesKeys]

export const emptyValues: ParsedProperties = {
  ...elementPropertiesEmptyValues,
  ...cssEmptyValues,
  ...layoutEmptyValues,
  ...layoutEmptyValuesNew,
}

export const computedStyleKeys: Array<string> = Object.keys({
  ...elementPropertiesEmptyValuesExcludingEvents,
  ...cssEmptyValues,
  ...layoutEmptyValuesNew,
})

type Parser<T> = (simpleValue: unknown, rawValue: ModifiableAttribute | null) => Either<string, T>

type ParseFunction<T, K extends keyof T> = (
  prop: K,
  maybeValue: unknown,
  maybeRawValue: ModifiableAttribute | null,
) => Either<string, ValueOf<T>>

function parseValueFactory<T, K extends keyof T>(parserMap: {
  [key in keyof T]: Parser<T[key]>
}): ParseFunction<T, K> {
  return (prop: K, maybeValue: unknown, maybeRawValue: ModifiableAttribute | null) => {
    try {
      return parserMap[prop](maybeValue, maybeRawValue)
    } catch (e) {
      return left(`Failed to parse value for property ${JSON.stringify(prop)}: ${e}`)
    }
  }
}

const parseMetadataValue = memoize(parseValueFactory(elementPropertiesParsers), {
  maxSize: 1000,
})
const parseCSSValue = memoize(parseValueFactory(cssParsers), { maxSize: 1000 })
const parseOldLayoutValue = memoize(parseValueFactory(layoutParsers), { maxSize: 1000 })
const parseNewLayoutValue = memoize(parseValueFactory(layoutParsersNew), { maxSize: 1000 })

function isMetadataProp(prop: unknown): prop is keyof ParsedElementProperties {
  return typeof prop === 'string' && prop in elementPropertiesEmptyValues
}

function isCSSProp(prop: unknown): prop is keyof ParsedCSSProperties {
  return typeof prop === 'string' && prop in cssEmptyValues
}

function isOldLayoutProp(prop: unknown): prop is keyof ParsedLayoutProperties {
  return typeof prop === 'string' && prop in layoutEmptyValues
}

function isNewLayoutProp(prop: unknown): prop is keyof LayoutPropertyTypes {
  return typeof prop === 'string' && prop in layoutEmptyValuesNew
}

// FIXME This function needs to die. The hooks should only be using the parser specific to the its input
export function parseAnyParseableValue<K extends keyof ParsedProperties>(
  prop: K,
  maybeValue: unknown,
  maybeRawValue: ModifiableAttribute | null,
): Either<string, ValueOf<ParsedProperties>> {
  try {
    // Find the right parser for this prop - I tried extracting this but TS couldn't deal with the downcasting and upcasting
    if (isMetadataProp(prop)) {
      return parseMetadataValue(prop, maybeValue, maybeRawValue)
    } else if (isNewLayoutProp(prop)) {
      return parseNewLayoutValue(prop, maybeValue, maybeRawValue)
    } else if (isOldLayoutProp(prop)) {
      return parseOldLayoutValue(prop, maybeValue, maybeRawValue)
    } else if (isCSSProp(prop)) {
      return parseCSSValue(prop, maybeValue, maybeRawValue)
    } else {
      return left(`Unable to find a parser for prop ${prop}`)
    }
  } catch (e) {
    return left(`Failed to parse value for property ${prop}: ${e}`)
  }
}

// hmmmm
type PrintedValue = JSExpression

type Printer<V extends ValueOf<ParsedProperties>> = (value: V) => PrintedValue

interface Printers extends MetadataPrinters, CSSPrinters, LayoutPrinters, LayoutPrintersNew {}

const printers: Printers = {
  ...elementPropertiesPrinters,
  ...cssPrinters,
  ...layoutPrinters,
  ...layoutPrintersNew,
}

export function printCSSValue<K extends keyof Printers, V extends ParsedProperties[K]>(
  prop: K,
  value: V,
): PrintedValue {
  // FIXME This is better than what we had but still not type safe!
  const printer = printers[prop] as Printer<V>
  return printer(value)
}

export function maybePrintCSSValue(prop: string, value: unknown): PrintedValue | unknown {
  // FIXME And you thought that wasn't type safe o_O
  const printerKey = prop as keyof Printers
  const printer = printers[printerKey] as Printer<any>
  if (printer == null) {
    return value
  } else {
    return printer(value as any)
  }
}

export const StyleProperties = [
  'backgroundColor',
  'backgroundImage',
  'backgroundSize',
  'border',
  'borderRadius',
  'boxShadow',
  'color',
  'opacity',
  'fontFamily',
  'fontSize',
  'fontStyle',
  'fontWeight',
  'lineHeight',
  'textDecoration',
  'textAlign',
  'textShadow',
]

export const LayoutPropertyList = [
  'left',
  'right',
  'top',
  'bottom',
  'width',
  'height',
  'position',
  'float',
  'min-width',
  'min-height',
  'max-width',
  'max-height',
  'margin',
  'margin-left',
  'margin-right',
  'margin-top',
  'margin-bottom',
  'padding',
  'padding-left',
  'padding-right',
  'padding-top',
  'padding-bottom',
  'transform',
  'object-fit',
  'column-count',
  'column-gap',
  'column-rule-style',
  'column-rule-width',
  'column-rule-color',
  'column-rule',
  'column-span',
  'column-width',
  'resize',
  'overflow',
  'box-sizing',
  'display',
  'order',
  'flex',
  'flex-grow',
  'flex-shrink',
  'flex-basis',
  'flex-direction',
  'flex-wrap',
  'flex-flow',
  'align-items',
  'align-content',
  'align-self',
  'justify-content',
  'justify-items',
  'justify-self',
  'gap',
  'row-gap',
  'column-gap',
  'grid-template-rows',
  'grid-template-columns',
  'grid-template-areas',
  'grid-auto-rows',
  'grid-auto-columns',
  'grid-auto-flow',
  'grid',
  'grid-area',
  'grid-auto-columns',
  'grid-auto-flow',
  'grid-auto-rows',
  'grid-column',
  'grid-column-end',
  'grid-column-start',
  'grid-row',
  'grid-row-end',
  'grid-row-start',
  'grid-template',
  'grid-template-areas',
  'grid-template-columns',
  'grid-template-rows',
]

export function isLayoutPropDetectedInCSS(cssProps: { [key: string]: any }): boolean {
  return LayoutPropertyList.findIndex((prop: string) => cssProps[prop] != null) > -1
}

interface NonTrivialKeyword {
  trivial: false
}

const nontrivial: NonTrivialKeyword = { trivial: false }

type ParsedPropertiesWithNonTrivial = {
  [Prop in keyof ParsedProperties]: ParsedProperties[Prop] | NonTrivialKeyword
}

export const trivialDefaultValues: ParsedPropertiesWithNonTrivial = {
  // ParsedCSSProperties
  backgroundColor: cssDefault(emptyBackgroundColor),
  backgroundImage: [],
  backgroundSize: [],
  border: emptyCssBorderDefault,
  borderRadius: {
    type: 'LEFT',
    value: {
      value: 0,
      unit: 'px',
    },
  },
  borderTopLeftRadius: {
    value: 0,
    unit: 'px',
  },
  borderTopRightRadius: {
    value: 0,
    unit: 'px',
  },
  borderBottomLeftRadius: {
    value: 0,
    unit: 'px',
  },
  borderBottomRightRadius: {
    value: 0,
    unit: 'px',
  },
  boxShadow: [],
  color: nontrivial,
  fontFamily: nontrivial,
  fontSize: nontrivial,
  fontStyle: 'normal',
  fontWeight: 400,
  letterSpacing: 'normal',
  lineHeight: 'normal',
  mixBlendMode: 'normal',
  opacity: nontrivial,
  overflow: nontrivial,
  textAlign: 'left',
  textDecorationColor: undefined,
  textDecorationLine: 'none',
  textDecorationStyle: 'solid',
  textShadow: [],
  transform: [],
  transformOrigin: {
    x: CSSTransformOriginStringValueX.Center,
    y: CSSTransformOriginStringValueY.Center,
  },

  objectFit: 'fill',

  flexWrap: FlexWrap.NoWrap,
  flexDirection: 'row',
  alignItems: FlexAlignment.FlexStart,
  alignContent: FlexAlignment.FlexStart,
  justifyContent: FlexJustifyContent.FlexStart,
  padding: nontrivial,
  paddingTop: {
    value: 0,
    unit: 'px',
  },
  paddingRight: {
    value: 0,
    unit: 'px',
  },
  paddingBottom: {
    value: 0,
    unit: 'px',
  },
  paddingLeft: {
    value: 0,
    unit: 'px',
  },
  alignSelf: FlexAlignment.Auto,
  position: 'relative',
  left: nontrivial, // nontrivial means we will never treat these props as "do not show if it equals default value"
  top: nontrivial,
  right: nontrivial,
  bottom: nontrivial,
  minWidth: {
    value: 0,
    unit: 'px',
  },
  maxWidth: undefined,
  minHeight: {
    value: 0,
    unit: 'px',
  },
  maxHeight: undefined,
  margin: nontrivial,
  marginTop: {
    value: 0,
    unit: 'px',
  },
  marginRight: {
    value: 0,
    unit: 'px',
  },
  marginBottom: {
    value: 0,
    unit: 'px',
  },
  marginLeft: {
    value: 0,
    unit: 'px',
  },
  flex: nontrivial,
  flexGrow: nontrivial,
  flexShrink: nontrivial,
  display: 'block',

  // ParsedElementProperties
  alt: '',
  src: '/',
  ...DOMEventHandlerEmptyValues,
  className: '',

  // ParsedLayoutProperties
  pinLeft: undefined,
  pinRight: undefined,
  centerX: undefined,
  width: undefined,
  pinTop: undefined,
  pinBottom: undefined,
  centerY: undefined,
  height: undefined,
  gapMain: { value: 0, unit: null },
  flexBasis: undefined,
  gap: {
    value: 0,
    unit: 'px',
  },
  zIndex: undefined,
}

export function isTrivialDefaultValue(
  propertyKey: ParsedPropertiesKeys,
  valueToCheck: ValueOf<ParsedProperties>,
): boolean {
  const maybeTrivial = trivialDefaultValues[propertyKey]
  return fastDeepEqual(maybeTrivial, valueToCheck)
}

export function toggleBorderEnabled(_: null, oldValue: CSSBorder): CSSBorder {
  const valueIsEnabled = (oldValue.style?.value.value ?? 'none') !== 'none'
  if (valueIsEnabled) {
    let workingNewValue = { ...oldValue }
    delete workingNewValue.style
    return workingNewValue
  } else {
    const widthValue =
      oldValue.width != null && isCSSNumber(oldValue.width.value) && oldValue.width.value.value > 0
        ? oldValue.width
        : cssLineWidth(cssNumber(1, 'px'))
    return {
      ...oldValue,
      width: widthValue,
      style: cssLineStyle(cssKeyword('solid')),
    }
  }
}

export function toggleShadowEnabled(oldValue: CSSBoxShadow): CSSBoxShadow {
  const newValue = { ...oldValue }
  newValue.enabled = !newValue.enabled
  return newValue
}
