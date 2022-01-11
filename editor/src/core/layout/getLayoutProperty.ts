// FIXME These functions create a circular ref

import { Either, flatMapEither, foldEither, right } from '../shared/either'
import { getSimpleAttributeAtPath, PropsOrJSXAttributes } from '../model/element-metadata-utils'
import { StyleLayoutProp } from './layout-helpers-new'
import { cssParsers, ParsedCSSProperties } from '../../components/inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../components/inspector/common/property-path-hooks'

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
