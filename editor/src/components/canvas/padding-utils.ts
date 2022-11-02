import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { defaultEither, Either, flatMapEither, isLeft, left, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { CanvasVector } from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSNumber, CSSPadding } from '../inspector/common/css-utils'
import { EdgePiece } from './canvas-types'

type CSSPaddingKey = keyof CSSPadding
export type SimpleCSSPadding = { [key in CSSPaddingKey]: PaddingMeasurement }

interface PaddingMeasurement {
  value: CSSNumber
  renderedDimension: number
}

export const defaultPaddingMeasurement = (sizePx: number): PaddingMeasurement => ({
  value: { value: sizePx, unit: null },
  renderedDimension: sizePx,
})

export function simplePaddingFromMetadata(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): SimpleCSSPadding {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const paddingFromMeasurements = element?.specialSizeMeasurements.padding

  const defaultPadding: SimpleCSSPadding = {
    paddingTop: defaultPaddingMeasurement(paddingFromMeasurements?.top ?? 0),
    paddingBottom: defaultPaddingMeasurement(paddingFromMeasurements?.bottom ?? 0),
    paddingLeft: defaultPaddingMeasurement(paddingFromMeasurements?.left ?? 0),
    paddingRight: defaultPaddingMeasurement(paddingFromMeasurements?.right ?? 0),
  }

  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return defaultPadding
  }

  const padding = cssPaddingToMeasurement(
    getLayoutProperty('padding', right(element.element.value.props), ['style']),
    defaultPadding,
  )

  const paddingTop = paddingMeasurementFromEither(
    getLayoutProperty('paddingTop', right(element.element.value.props), ['style']),
    defaultPadding.paddingTop,
  )

  const paddingBottom = paddingMeasurementFromEither(
    getLayoutProperty('paddingBottom', right(element.element.value.props), ['style']),
    defaultPadding.paddingBottom,
  )

  const paddingLeft = paddingMeasurementFromEither(
    getLayoutProperty('paddingLeft', right(element.element.value.props), ['style']),
    defaultPadding.paddingLeft,
  )

  const paddingRight = paddingMeasurementFromEither(
    getLayoutProperty('paddingRight', right(element.element.value.props), ['style']),
    defaultPadding.paddingRight,
  )

  return cssPaddingWithDefaults(
    { paddingTop, paddingBottom, paddingLeft, paddingRight },
    padding,
    defaultPadding,
  )
}

function cssPaddingWithDefaults(
  parts: Partial<SimpleCSSPadding>,
  whole: Partial<SimpleCSSPadding>,
  defaults: SimpleCSSPadding,
): SimpleCSSPadding {
  return {
    paddingTop: parts.paddingTop ?? whole.paddingTop ?? defaults.paddingTop,
    paddingBottom: parts.paddingBottom ?? whole.paddingBottom ?? defaults.paddingBottom,
    paddingLeft: parts.paddingLeft ?? whole.paddingLeft ?? defaults.paddingLeft,
    paddingRight: parts.paddingRight ?? whole.paddingRight ?? defaults.paddingRight,
  }
}

function paddingMeasurementFromEither(
  value: Either<string, CSSNumber | undefined>,
  defaultMeasurement: PaddingMeasurement,
): PaddingMeasurement {
  const paddingMeasurementFromCSSNumber = (
    number: CSSNumber | undefined,
  ): Either<undefined, PaddingMeasurement> =>
    number == null
      ? left(undefined)
      : right({ renderedDimension: defaultMeasurement.renderedDimension, value: number })

  return defaultEither(defaultMeasurement, flatMapEither(paddingMeasurementFromCSSNumber, value))
}

function cssPaddingToMeasurement(
  p: Either<string, CSSPadding | undefined>,
  padding: SimpleCSSPadding,
): Partial<SimpleCSSPadding> {
  if (isLeft(p) || p.value == null) {
    return padding
  }

  return {
    paddingTop: {
      value: p.value.paddingTop,
      renderedDimension: padding.paddingTop.renderedDimension,
    },
    paddingBottom: {
      value: p.value.paddingBottom,
      renderedDimension: padding.paddingBottom.renderedDimension,
    },
    paddingLeft: {
      value: p.value.paddingLeft,
      renderedDimension: padding.paddingLeft.renderedDimension,
    },
    paddingRight: {
      value: p.value.paddingRight,
      renderedDimension: padding.paddingRight.renderedDimension,
    },
  }
}

export function paddingForEdge(edgePiece: EdgePiece, padding: SimpleCSSPadding): number {
  switch (edgePiece) {
    case 'top':
      return padding.paddingTop.renderedDimension
    case 'bottom':
      return padding.paddingBottom.renderedDimension
    case 'right':
      return padding.paddingRight.renderedDimension
    case 'left':
      return padding.paddingLeft.renderedDimension
    default:
      assertNever(edgePiece)
  }
}

const offsetMeasurementByDelta = (
  measurement: PaddingMeasurement,
  delta: number,
): PaddingMeasurement => {
  const pixelsPerUnit = measurement.value.value / measurement.renderedDimension
  const deltaInUnits = delta * pixelsPerUnit
  return {
    renderedDimension: measurement.renderedDimension + delta,
    value: {
      unit: measurement.value.unit,
      value: measurement.value.value + deltaInUnits,
    },
  }
}

export function offsetPaddingByEdge(
  edge: EdgePiece,
  delta: number,
  padding: SimpleCSSPadding,
): SimpleCSSPadding {
  switch (edge) {
    case 'bottom':
      return { ...padding, paddingBottom: offsetMeasurementByDelta(padding.paddingBottom, delta) }
    case 'top':
      return { ...padding, paddingTop: offsetMeasurementByDelta(padding.paddingTop, delta) }
    case 'left':
      return { ...padding, paddingLeft: offsetMeasurementByDelta(padding.paddingLeft, delta) }
    case 'right':
      return { ...padding, paddingRight: offsetMeasurementByDelta(padding.paddingRight, delta) }
    default:
      assertNever(edge)
  }
}

function paddingMeasurementToString(measurement: PaddingMeasurement): string {
  return `${measurement.value.value}${measurement.value.unit ?? 'px'}`
}

export function paddingToPaddingString(padding: SimpleCSSPadding): string {
  return [
    paddingMeasurementToString(padding.paddingTop),
    paddingMeasurementToString(padding.paddingRight),
    paddingMeasurementToString(padding.paddingBottom),
    paddingMeasurementToString(padding.paddingLeft),
  ].join(' ')
}

export function deltaFromEdge(delta: CanvasVector, edgePiece: EdgePiece): number {
  switch (edgePiece) {
    case 'top':
      return delta.y
    case 'bottom':
      return -delta.y
    case 'left':
      return delta.x
    case 'right':
      return -delta.x
    default:
      assertNever(edgePiece)
  }
}
