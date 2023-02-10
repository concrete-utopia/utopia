import { styleStringInArray } from '../../utils/common-constants'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { defaultEither, isLeft, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { CanvasVector } from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSNumber, CSSNumberUnit, CSSPadding, printCSSNumber } from '../inspector/common/css-utils'
import { EdgePiece } from './canvas-types'
import {
  AdjustPrecision,
  cssNumberWithRenderedValue,
  CSSNumberWithRenderedValue,
  offsetMeasurementByDelta,
  unitlessCSSNumberWithRenderedValue,
} from './controls/select-mode/controls-common'
import { Modifiers } from '../../utils/modifiers'

export const EdgePieces: Array<EdgePiece> = ['top', 'bottom', 'left', 'right']

export type CSSPaddingKey = keyof CSSPadding
export type CSSPaddingMappedValues<T> = { [key in CSSPaddingKey]: T }
export type CSSPaddingMeasurements = CSSPaddingMappedValues<CSSNumberWithRenderedValue>

export function paddingFromSpecialSizeMeasurements(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSPaddingMappedValues<number> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const paddingFromMeasurements = element?.specialSizeMeasurements.padding

  const paddingMappedMeasurements: CSSPaddingMappedValues<number> = {
    paddingTop: paddingFromMeasurements?.top ?? 0,
    paddingBottom: paddingFromMeasurements?.bottom ?? 0,
    paddingLeft: paddingFromMeasurements?.left ?? 0,
    paddingRight: paddingFromMeasurements?.right ?? 0,
  }

  return paddingMappedMeasurements
}

export function simplePaddingFromMetadata(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const defaults: CSSPaddingMappedValues<CSSNumber | undefined> = {
    paddingTop: undefined,
    paddingRight: undefined,
    paddingBottom: undefined,
    paddingLeft: undefined,
  }

  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return {
      paddingTop: undefined,
      paddingRight: undefined,
      paddingBottom: undefined,
      paddingLeft: undefined,
    }
  }

  const paddingNumbers = paddingFromSpecialSizeMeasurements(metadata, elementPath)

  const padding: CSSPadding | undefined = defaultEither(
    undefined,
    getLayoutProperty('padding', right(element.element.value.props), styleStringInArray),
  )

  const paddingLonghands: CSSPaddingMappedValues<CSSNumber | undefined> = {
    paddingTop: defaultEither(
      undefined,
      getLayoutProperty('paddingTop', right(element.element.value.props), styleStringInArray),
    ),
    paddingBottom: defaultEither(
      undefined,
      getLayoutProperty('paddingBottom', right(element.element.value.props), styleStringInArray),
    ),
    paddingLeft: defaultEither(
      undefined,
      getLayoutProperty('paddingLeft', right(element.element.value.props), styleStringInArray),
    ),
    paddingRight: defaultEither(
      undefined,
      getLayoutProperty('paddingRight', right(element.element.value.props), styleStringInArray),
    ),
  }

  const make = (prop: CSSPaddingKey): CSSNumberWithRenderedValue | undefined => {
    return (
      optionalMap(
        (p) => cssNumberWithRenderedValue(p, paddingNumbers[prop]),
        paddingLonghands[prop],
      ) ??
      optionalMap(
        (p) => cssNumberWithRenderedValue(p, paddingNumbers[prop]),
        padding?.[prop] ?? defaults[prop],
      ) ??
      undefined
    )
  }

  return {
    paddingTop: make('paddingTop'),
    paddingRight: make('paddingRight'),
    paddingBottom: make('paddingBottom'),
    paddingLeft: make('paddingLeft'),
  }
}

export function combinePaddings(
  paddingNumber: CSSPaddingMappedValues<number>,
  paddingRenderedValues: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue> {
  return {
    paddingTop:
      paddingRenderedValues.paddingTop ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingTop),
    paddingRight:
      paddingRenderedValues.paddingRight ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingRight),
    paddingBottom:
      paddingRenderedValues.paddingBottom ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingBottom),
    paddingLeft:
      paddingRenderedValues.paddingLeft ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingLeft),
  }
}

export function maybeFullPadding(
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue> | undefined {
  const { paddingTop, paddingRight, paddingBottom, paddingLeft } = padding
  if (paddingTop == null || paddingRight == null || paddingBottom == null || paddingLeft == null) {
    return undefined
  }
  return { paddingTop, paddingRight, paddingBottom, paddingLeft }
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

export function paddingForEdgeSimplePadding(
  edgePiece: EdgePiece,
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): number {
  return padding[paddingPropForEdge(edgePiece)]?.renderedValuePx ?? 0
}

export function offsetPaddingByEdge(
  prop: CSSPaddingKey,
  delta: number,
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
  precision: AdjustPrecision,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined> {
  return {
    ...padding,
    [prop]: offsetMeasurementByDelta(
      padding[prop] ?? unitlessCSSNumberWithRenderedValue(0),
      delta,
      precision,
    ),
  }
}

export function paddingToPaddingString(padding: CSSPaddingMeasurements): string {
  return [
    printCssNumberWithDefaultUnit(padding.paddingTop.value, 'px'),
    printCssNumberWithDefaultUnit(padding.paddingRight.value, 'px'),
    printCssNumberWithDefaultUnit(padding.paddingBottom.value, 'px'),
    printCssNumberWithDefaultUnit(padding.paddingLeft.value, 'px'),
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

export function printCssNumberWithDefaultUnit(
  value: CSSNumber,
  defaultUnit?: CSSNumberUnit,
): string | number {
  if (defaultUnit != null && value.unit == null) {
    return printCSSNumber({ value: value.value, unit: defaultUnit }, null)
  }
  return printCSSNumber(value, null)
}

export type PaddingAdjustMode = 'individual' | 'cross-axis' | 'all'
export function paddingAdjustMode(modifiers: Modifiers): PaddingAdjustMode {
  if (modifiers.shift === true && modifiers.alt === true) {
    return 'all'
  }
  if (modifiers.alt === true) {
    return 'cross-axis'
  }
  return 'individual'
}
