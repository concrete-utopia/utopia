import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { Either, isLeft, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { CanvasVector } from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSNumber, CSSPadding } from '../inspector/common/css-utils'
import { EdgePiece } from './canvas-types'
import {
  AdjustPrecision,
  CSSNumberWithRenderedValue,
  offsetMeasurementByDelta,
} from './controls/select-mode/controls-common'

export type CSSPaddingKey = keyof CSSPadding
export type CSSPaddingMappedValues<T> = { [key in CSSPaddingKey]: T }
export type CSSPaddingMeasurements = CSSPaddingMappedValues<CSSNumberWithRenderedValue>

export const defaultPaddingMeasurement = (sizePx: number): CSSNumberWithRenderedValue => ({
  value: { value: sizePx, unit: null },
  renderedValuePx: sizePx,
})

export function simplePaddingFromMetadata(
  metadata: ElementInstanceMetadataMap,
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
    getLayoutProperty('padding', right(element.element.value.props), ['style']),
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
): CSSNumberWithRenderedValue | undefined {
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

export function paddingPropForEdge(edgePiece: EdgePiece): CSSPaddingKey {
  switch (edgePiece) {
    case 'top':
      return 'paddingTop'
    case 'bottom':
      return 'paddingBottom'
    case 'right':
      return 'paddingRight'
    case 'left':
      return 'paddingLeft'
    default:
      assertNever(edgePiece)
  }
}

export function paddingForEdge(edgePiece: EdgePiece, padding: CSSPaddingMeasurements): number {
  return padding[paddingPropForEdge(edgePiece)].renderedValuePx
}

export function offsetPaddingByEdge(
  edge: EdgePiece,
  delta: number,
  padding: CSSPaddingMeasurements,
  precision: AdjustPrecision,
): CSSPaddingMeasurements {
  switch (edge) {
    case 'bottom':
      return {
        ...padding,
        paddingBottom: offsetMeasurementByDelta(padding.paddingBottom, delta, precision),
      }
    case 'top':
      return {
        ...padding,
        paddingTop: offsetMeasurementByDelta(padding.paddingTop, delta, precision),
      }
    case 'left':
      return {
        ...padding,
        paddingLeft: offsetMeasurementByDelta(padding.paddingLeft, delta, precision),
      }
    case 'right':
      return {
        ...padding,
        paddingRight: offsetMeasurementByDelta(padding.paddingRight, delta, precision),
      }
    default:
      assertNever(edge)
  }
}

function paddingMeasurementToString(measurement: CSSNumberWithRenderedValue): string {
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

export const PaddingIndictorOffset = (scale: number): number => 10 / scale
