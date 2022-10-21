import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { Either, isLeft, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSNumber, cssNumber, CSSPadding } from '../inspector/common/css-utils'
import { EdgePiece } from './canvas-types'

type CSSPaddingKey = keyof CSSPadding
export type SimpleCSSPadding = { [key in CSSPaddingKey]: number }

export function simplePaddingFromMetadata(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): SimpleCSSPadding {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const defaultPadding: SimpleCSSPadding = {
    paddingTop: 0,
    paddingBottom: 0,
    paddingLeft: 0,
    paddingRight: 0,
  }

  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return defaultPadding
  }

  const padding = cssPaddingToSimple(
    getLayoutProperty('padding', right(element.element.value.props), ['style']),
    defaultPadding,
  )

  const paddingTop = pxValueFromEither(
    getLayoutProperty('paddingTop', right(element.element.value.props), ['style']),
  )

  const paddingBottom = pxValueFromEither(
    getLayoutProperty('paddingBottom', right(element.element.value.props), ['style']),
  )

  const paddingLeft = pxValueFromEither(
    getLayoutProperty('paddingLeft', right(element.element.value.props), ['style']),
  )

  const paddingRight = pxValueFromEither(
    getLayoutProperty('paddingRight', right(element.element.value.props), ['style']),
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

const pxValue = (number: CSSNumber): number | undefined =>
  number.unit === 'px' || number.unit == null ? number.value : undefined

function pxValueFromEither(value: Either<string, CSSNumber | undefined>): number | undefined {
  if (isLeft(value) || value.value == null) {
    return undefined
  }
  return pxValue(value.value)
}

function cssPaddingToSimple(
  p: Either<string, CSSPadding | undefined>,
  padding: SimpleCSSPadding,
): Partial<SimpleCSSPadding> {
  if (isLeft(p) || p.value == null) {
    return padding
  }

  return {
    paddingTop: pxValue(p.value.paddingTop),
    paddingBottom: pxValue(p.value.paddingBottom),
    paddingLeft: pxValue(p.value.paddingLeft),
    paddingRight: pxValue(p.value.paddingRight),
  }
}

export function paddingForEdge(edgePiece: EdgePiece, padding: SimpleCSSPadding): number {
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

export function offsetPaddingByEdge(
  edge: EdgePiece,
  delta: number,
  padding: SimpleCSSPadding,
): SimpleCSSPadding {
  switch (edge) {
    case 'bottom':
      return { ...padding, paddingBottom: padding.paddingBottom + delta }
    case 'top':
      return { ...padding, paddingTop: padding.paddingTop + delta }
    case 'left':
      return { ...padding, paddingLeft: padding.paddingLeft + delta }
    case 'right':
      return { ...padding, paddingRight: padding.paddingRight + delta }
    default:
      assertNever(edge)
  }
}

export function paddingToPaddingString(padding: SimpleCSSPadding): string {
  return `${padding.paddingTop}px ${padding.paddingRight}px ${padding.paddingBottom}px ${padding.paddingLeft}px`
}
