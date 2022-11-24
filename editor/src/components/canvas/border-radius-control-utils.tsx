import { Sides } from 'utopia-api/core'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { foldEither, defaultEither, isLeft, isRight, right } from '../../core/shared/either'
import {
  ElementInstanceMetadata,
  isIntrinsicElement,
  jsxElementNameEquals,
  jsxElementName,
  JSXAttributes,
  isJSXElement,
  JSXElement,
} from '../../core/shared/element-template'
import { CanvasPoint, canvasPoint, clamp, roundTo, size, Size } from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { assertNever } from '../../core/shared/utils'
import {
  CSSBorderRadius,
  CSSBorderRadiusIndividual,
  CSSNumber,
  cssNumber,
} from '../inspector/common/css-utils'
import {
  CSSNumberWithRenderedValue,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
  cssNumberEqual,
  cssNumberWithRenderedValue,
  measurementBasedOnOtherMeasurement,
} from './controls/select-mode/controls-common'

export type BorderRadiusCorner = keyof CSSBorderRadiusIndividual

export const BorderRadiusCorners: BorderRadiusCorner[] = ['tl', 'tr', 'bl', 'br']

const AllSides: Array<keyof Sides> = ['bottom', 'left', 'right', 'top']

export type BorderRadiusCornerMapped<T> = { [key in BorderRadiusCorner]: T }

export const BorderRadiusControlMinimumForDisplay = (scale: number): number => 12 / scale

export type BorderRadiusAdjustMode = 'individual' | 'all'

export const BorderRadiusHandleSize = (scale: number): number => 8 / scale

export const BorderRadiusHandleDotSize = (scale: number): number => 2 / scale

export const BorderRadiusHandleBorderWidth = (scale: number): number => 1 / scale

export const BorderRadiusHandleHitArea = (scale: number): number => 20 / scale

export const maxBorderRadius = (elementSize: Size): number =>
  roundTo(Math.min(elementSize.height, elementSize.width) / 2, 0)

export function handlePosition(
  offset: number,
  elementSize: Size,
  corner: BorderRadiusCorner,
  scale: number,
): CanvasPoint {
  const handleSize = BorderRadiusHandleSize(scale) / 2
  switch (corner) {
    case 'tl':
      return canvasPoint({ x: offset - handleSize, y: offset - handleSize })
    case 'tr':
      return canvasPoint({ x: elementSize.width - offset - handleSize, y: offset - handleSize })
    case 'bl':
      return canvasPoint({ x: offset - handleSize, y: elementSize.height - offset - handleSize })
    case 'br':
      return canvasPoint({
        x: elementSize.width - offset - handleSize,
        y: elementSize.height - offset - handleSize,
      })
    default:
      assertNever(corner)
  }
}

export function borderRadiusFromElement(
  element: ElementInstanceMetadata,
): BorderRadiusData<CSSNumberWithRenderedValue> | null {
  const jsxElement: JSXElement | null = foldEither(
    () => null,
    (e) => (isJSXElement(e) ? e : null),
    element.element,
  )

  if (jsxElement == null) {
    return null
  }

  const renderedValueSides = element.specialSizeMeasurements.borderRadius
  if (renderedValueSides == null) {
    return null
  }

  const fromProps = borderRadiusFromProps(jsxElement.props)
  const measurementsNonZero = AllSides.some((c) => (renderedValueSides[c] ?? 0) > 0)

  const elementIsIntrinsicElementOrScene = foldEither(
    () => false,
    (e) =>
      isJSXElement(e) &&
      (isIntrinsicElement(e.name) || jsxElementNameEquals(e.name, jsxElementName('Scene', []))),
    element.element,
  )

  if (
    !(
      elementIsIntrinsicElementOrScene ||
      shouldShowControls({
        propAvailableFromStyle: fromProps != null,
        measurementsNonZero: measurementsNonZero,
      })
    )
  ) {
    return null
  }

  const borderRadius = optionalMap(
    (radius) => measurementFromBorderRadius(renderedValueSides, radius),
    fromProps,
  )

  if (borderRadius == null) {
    if (!elementIsIntrinsicElementOrScene) {
      return {
        mode: 'all',
        borderRadius: borderRadiusSidesFromValue(unitlessCSSNumberWithRenderedValue(0)),
      }
    }
    return null
  }

  const borderRadiusUpperLimit = maxBorderRadius(
    size(element.specialSizeMeasurements.clientWidth, element.specialSizeMeasurements.clientHeight),
  )

  return {
    mode: fromProps?.type === 'sides' ? 'individual' : 'all',
    borderRadius: mapBorderRadiusSides(
      (n) => adjustBorderRadius({ min: 0, max: borderRadiusUpperLimit }, n),
      borderRadius,
    ),
  }
}

interface BorderRadiusFromProps {
  type: 'sides' | 'borderRadius'
  sides: BorderRadiusCornerMapped<CSSNumber>
}

function borderRadiusFromProps(props: JSXAttributes): BorderRadiusFromProps | null {
  const simpleBorderRadius = simpleBorderRadiusFromProps(props)

  const borderTopLeftRadius = defaultEither(
    null,
    getLayoutProperty('borderTopLeftRadius', right(props), ['style']),
  )
  const borderTopRightRadius = defaultEither(
    null,
    getLayoutProperty('borderTopRightRadius', right(props), ['style']),
  )
  const borderBottomLeftRadius = defaultEither(
    null,
    getLayoutProperty('borderBottomLeftRadius', right(props), ['style']),
  )
  const borderBottomRightRadius = defaultEither(
    null,
    getLayoutProperty('borderBottomRightRadius', right(props), ['style']),
  )

  if (
    borderTopLeftRadius != null ||
    borderTopRightRadius != null ||
    borderBottomLeftRadius != null ||
    borderBottomRightRadius != null
  ) {
    return {
      type: 'sides',
      sides: {
        tl: borderTopLeftRadius ?? simpleBorderRadius?.tl ?? cssNumber(0),
        tr: borderTopRightRadius ?? simpleBorderRadius?.tr ?? cssNumber(0),
        bl: borderBottomLeftRadius ?? simpleBorderRadius?.bl ?? cssNumber(0),
        br: borderBottomRightRadius ?? simpleBorderRadius?.br ?? cssNumber(0),
      },
    }
  }

  if (simpleBorderRadius != null) {
    const { tl, tr, bl, br } = simpleBorderRadius
    const allSidesEqual = [tr, bl, br].every((c) => cssNumberEqual(tl, c))

    return {
      type: allSidesEqual ? 'borderRadius' : 'sides',
      sides: simpleBorderRadius,
    }
  }

  return null
}

export interface BorderRadiusData<T> {
  mode: BorderRadiusAdjustMode
  borderRadius: BorderRadiusCornerMapped<T>
}

function simpleBorderRadiusFromProps(
  props: JSXAttributes,
): BorderRadiusCornerMapped<CSSNumber> | null {
  const borderRadius = getLayoutProperty('borderRadius', right(props), ['style'])
  if (isRight(borderRadius) && borderRadius.value != null) {
    return borderRadiusIndividualFromCSSBorderRadius(borderRadius.value)
  }
  return null
}

function borderRadiusIndividualFromCSSBorderRadius(
  borderRadius: CSSBorderRadius,
): BorderRadiusCornerMapped<CSSNumber> {
  return isLeft(borderRadius) ? borderRadiusSidesFromValue(borderRadius.value) : borderRadius.value
}

export function sizeFromElement(element: ElementInstanceMetadata): Size {
  return size(element.globalFrame?.width ?? 0, element.globalFrame?.height ?? 0)
}

function measurementFromBorderRadius(
  sides: Sides,
  borderRadius: BorderRadiusFromProps,
): BorderRadiusCornerMapped<CSSNumberWithRenderedValue> | null {
  return {
    tl: cssNumberWithRenderedValue(borderRadius.sides.tl, sides.top ?? 0),
    tr: cssNumberWithRenderedValue(borderRadius.sides.tr, sides.right ?? 0),
    bl: cssNumberWithRenderedValue(borderRadius.sides.bl, sides.bottom ?? 0),
    br: cssNumberWithRenderedValue(borderRadius.sides.br, sides.left ?? 0),
  }
}

export function mapBorderRadiusSides<T, U>(
  f: (_: T) => U,
  sides: BorderRadiusCornerMapped<T>,
): BorderRadiusCornerMapped<U> {
  return {
    tl: f(sides.tl),
    tr: f(sides.tr),
    bl: f(sides.bl),
    br: f(sides.br),
  }
}

function borderRadiusSidesFromValue<T>(radius: T): BorderRadiusCornerMapped<T> {
  return {
    tl: radius,
    tr: radius,
    bl: radius,
    br: radius,
  }
}

interface MinMax {
  min: number
  max: number
}

function adjustBorderRadius(
  { min, max }: MinMax,
  borderRadius: CSSNumberWithRenderedValue,
): CSSNumberWithRenderedValue {
  return measurementBasedOnOtherMeasurement(
    borderRadius,
    clamp(min, max, borderRadius.renderedValuePx),
    'precise',
  )
}
