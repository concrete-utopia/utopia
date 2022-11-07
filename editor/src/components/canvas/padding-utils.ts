import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { Either, isLeft, right } from '../../core/shared/either'
import { cssParsers } from '../../components/inspector/common/css-utils'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { CanvasVector } from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { AllElementProps } from '../editor/store/editor-state'
import { CSSNumber, CSSNumberUnit, CSSPadding } from '../inspector/common/css-utils'
import { EdgePiece } from './canvas-types'
import { toString } from '../../core/shared/element-path'

type CSSPaddingKey = keyof CSSPadding
type CSSPaddingMappedValues<T> = { [key in CSSPaddingKey]: T }
export type CSSPaddingMeasurements = CSSPaddingMappedValues<PaddingMeasurement>

export interface PaddingMeasurement {
  value: CSSNumber
  renderedValuePx: number
}

export const defaultPaddingMeasurement = (sizePx: number): PaddingMeasurement => ({
  value: { value: sizePx, unit: null },
  renderedValuePx: sizePx,
})

export function simplePaddingFromMetadata(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CSSPaddingMeasurements {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const paddingFromMeasurements = element?.specialSizeMeasurements.padding

  const paddingMappedMeasurements: CSSPaddingMappedValues<number> = {
    paddingTop: paddingFromMeasurements?.top ?? 0,
    paddingBottom: paddingFromMeasurements?.bottom ?? 0,
    paddingLeft: paddingFromMeasurements?.left ?? 0,
    paddingRight: paddingFromMeasurements?.right ?? 0,
  }

  const defaultPadding = {
    paddingTop: defaultPaddingMeasurement(paddingMappedMeasurements.paddingTop),
    paddingBottom: defaultPaddingMeasurement(paddingMappedMeasurements.paddingBottom),
    paddingLeft: defaultPaddingMeasurement(paddingMappedMeasurements.paddingLeft),
    paddingRight: defaultPaddingMeasurement(paddingMappedMeasurements.paddingRight),
  }

  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return defaultPadding
  }

  const padding = cssPaddingToMeasurement(
    getLayoutProperty('padding', right(allElementProps[toString(elementPath)]), ['style']),
    paddingMappedMeasurements,
  )

  const paddingTop = paddingMeasurementFromEither(
    getLayoutProperty('paddingTop', right(element.element.value.props), ['style']),
    paddingMappedMeasurements.paddingTop,
  )

  const paddingBottom = paddingMeasurementFromEither(
    getLayoutProperty('paddingBottom', right(element.element.value.props), ['style']),
    paddingMappedMeasurements.paddingBottom,
  )

  const paddingLeft = paddingMeasurementFromEither(
    getLayoutProperty('paddingLeft', right(element.element.value.props), ['style']),
    paddingMappedMeasurements.paddingLeft,
  )

  const paddingRight = paddingMeasurementFromEither(
    getLayoutProperty('paddingRight', right(element.element.value.props), ['style']),
    paddingMappedMeasurements.paddingRight,
  )

  return cssPaddingWithDefaults(
    { paddingTop, paddingBottom, paddingLeft, paddingRight },
    padding,
    defaultPadding,
  )
}

function cssPaddingWithDefaults(
  parts: Partial<CSSPaddingMeasurements>,
  whole: Partial<CSSPaddingMeasurements>,
  defaults: CSSPaddingMeasurements,
): CSSPaddingMeasurements {
  return {
    paddingTop: parts.paddingTop ?? whole.paddingTop ?? defaults.paddingTop,
    paddingBottom: parts.paddingBottom ?? whole.paddingBottom ?? defaults.paddingBottom,
    paddingLeft: parts.paddingLeft ?? whole.paddingLeft ?? defaults.paddingLeft,
    paddingRight: parts.paddingRight ?? whole.paddingRight ?? defaults.paddingRight,
  }
}

function paddingMeasurementFromEither(
  value: Either<string, CSSNumber | undefined>,
  renderedDimension: number,
): PaddingMeasurement | undefined {
  if (isLeft(value) || value.value == null) {
    return undefined
  }

  return {
    renderedValuePx: renderedDimension,
    value: value.value,
  }
}

function cssPaddingToMeasurement(
  p: Either<string, CSSPadding | undefined>,
  padding: CSSPaddingMappedValues<number>,
): Partial<CSSPaddingMeasurements> {
  if (isLeft(p) || p.value == null) {
    return {}
  }

  return {
    paddingTop: {
      value: p.value.paddingTop,
      renderedValuePx: padding.paddingTop,
    },
    paddingBottom: {
      value: p.value.paddingBottom,
      renderedValuePx: padding.paddingBottom,
    },
    paddingLeft: {
      value: p.value.paddingLeft,
      renderedValuePx: padding.paddingLeft,
    },
    paddingRight: {
      value: p.value.paddingRight,
      renderedValuePx: padding.paddingRight,
    },
  }
}

export function paddingForEdge(edgePiece: EdgePiece, padding: CSSPaddingMeasurements): number {
  switch (edgePiece) {
    case 'top':
      return padding.paddingTop.renderedValuePx
    case 'bottom':
      return padding.paddingBottom.renderedValuePx
    case 'right':
      return padding.paddingRight.renderedValuePx
    case 'left':
      return padding.paddingLeft.renderedValuePx
    default:
      assertNever(edgePiece)
  }
}

export function paddingMeasurementForEdge(
  edgePiece: EdgePiece,
  padding: CSSPaddingMeasurements,
): PaddingMeasurement {
  switch (edgePiece) {
    case 'top':
      return padding.paddingTop
    case 'bottom':
      return padding.paddingBottom
    case 'right':
      return padding.paddingRight
    case 'left':
      return padding.paddingLeft
    default:
      assertNever(edgePiece)
  }
}

function valueWithUnitAppropriatePrecision(unit: CSSNumberUnit | null, value: number): number {
  if (unit === 'em') {
    return Math.floor(value * 10) / 10
  }
  return Math.floor(value)
}

export const offsetMeasurementByDelta = (
  measurement: PaddingMeasurement,
  delta: number,
): PaddingMeasurement => {
  if (measurement.renderedValuePx === 0) {
    return measurement
  }
  const pixelsPerUnit = measurement.value.value / measurement.renderedValuePx
  const deltaInUnits = valueWithUnitAppropriatePrecision(
    measurement.value.unit,
    delta * pixelsPerUnit,
  )
  return {
    renderedValuePx: measurement.renderedValuePx + delta,
    value: {
      unit: measurement.value.unit,
      value: measurement.value.value + deltaInUnits,
    },
  }
}

export function offsetPaddingByEdge(
  edge: EdgePiece,
  delta: number,
  padding: CSSPaddingMeasurements,
): CSSPaddingMeasurements {
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

export function paddingToPaddingString(padding: CSSPaddingMeasurements): string {
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
