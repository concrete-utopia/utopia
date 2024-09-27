// FIXME These functions create a circular ref

import type { Either } from '../shared/either'
import { flatMapEither, foldEither, right } from '../shared/either'
import type { PropsOrJSXAttributes } from '../model/element-metadata-utils'
import { getSimpleAttributeAtPath } from '../model/element-metadata-utils'
import type { LayoutPinnedProp, StyleLayoutProp } from './layout-helpers-new'
import type { ParsedCSSProperties } from '../../components/inspector/common/css-utils'
import { cssParsers } from '../../components/inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../components/inspector/common/property-path-hooks'

export function applyParserToValue<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  layoutProp: P,
  value: unknown,
) {
  const parser = cssParsers[layoutProp] as (value: unknown) => Either<string, T>
  function applyParser(v: any): Either<string, T | undefined> {
    if (v === undefined) {
      return right(undefined)
    } else {
      return parser(v)
    }
  }

  return applyParser(value)
}

export function getLayoutProperty<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  layoutProp: P,
  propsOrAttributes: PropsOrJSXAttributes,
  propertyTarget: ReadonlyArray<string>,
): Either<string, T | undefined> {
  const path = stylePropPathMappingFn(layoutProp, propertyTarget)
  const parser = cssParsers[layoutProp] as (value: unknown) => Either<string, T>
  function applyParser(value: any): Either<string, T | undefined> {
    if (value === undefined) {
      return right(undefined)
    } else {
      return parser(value)
    }
  }
  return flatMapEither(applyParser, getSimpleAttributeAtPath(propsOrAttributes, path))
}

export function getLayoutPropertyOr<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  orValue: T,
  layoutProp: P,
  props: PropsOrJSXAttributes,
  propertyTarget: ReadonlyArray<string>,
): T {
  const layoutProperty = getLayoutProperty<P, T>(layoutProp, props, propertyTarget)
  return foldEither(
    (_) => orValue,
    (val) => (val === undefined ? orValue : val),
    layoutProperty,
  )
}

const MaxContent = 'max-content'
export function getLayoutLengthValueOrKeyword<
  P extends LayoutPinnedProp,
  T = ParsedCSSProperties[P],
>(
  layoutProp: P,
  propsOrAttributes: PropsOrJSXAttributes,
  propertyTarget: ReadonlyArray<string>,
): Either<string, T | typeof MaxContent | undefined> {
  const path = stylePropPathMappingFn(layoutProp, propertyTarget)
  if (layoutProp === 'width' || layoutProp === 'height') {
    const lengthPercentParser = cssParsers[layoutProp] as (value: unknown) => Either<string, T>
    function applyParser(value: any): Either<string, T | typeof MaxContent | undefined> {
      if (value === undefined) {
        return right(undefined)
      } else {
        if (value === MaxContent) {
          return right(value)
        } else {
          return lengthPercentParser(value)
        }
      }
    }
    return flatMapEither(applyParser, getSimpleAttributeAtPath(propsOrAttributes, path))
  } else {
    return getLayoutProperty(layoutProp, propsOrAttributes, propertyTarget)
  }
}
