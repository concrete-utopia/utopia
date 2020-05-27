// FIXME These functions create a circular ref

import { Either, foldEither } from '../shared/either'
import { getSimpleAttributeAtPath, PropsOrJSXAttributes } from '../model/element-metadata-utils'
import {
  LayoutProp,
  StyleLayoutProp,
  LayoutPropertyTypesAndCSSPropertyTypes,
  createLayoutPropertyPath,
} from './layout-helpers-new'

export function getLayoutProperty<
  P extends LayoutProp | StyleLayoutProp,
  T = LayoutPropertyTypesAndCSSPropertyTypes[P]
>(layoutProp: P, propsOrAttributes: PropsOrJSXAttributes): Either<string, T> {
  const path = createLayoutPropertyPath(layoutProp)
  return getSimpleAttributeAtPath(propsOrAttributes, path)
}

export function getLayoutPropertyOr<
  P extends LayoutProp | StyleLayoutProp,
  T = LayoutPropertyTypesAndCSSPropertyTypes[P]
>(orValue: T, layoutProp: P, props: PropsOrJSXAttributes): T {
  const layoutProperty = getLayoutProperty<P, T>(layoutProp, props)
  return foldEither(
    (_) => orValue,
    (val) => (val === undefined ? orValue : val),
    layoutProperty,
  )
}
