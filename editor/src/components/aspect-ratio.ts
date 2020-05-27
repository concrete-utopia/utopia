import { getValueOrDefaultFallbackForPath } from './inspector/schema-stuff'
import { ElementInstanceMetadata, isJSXElement } from '../core/shared/element-template'
import { isRight, isLeft, foldEither } from '../core/shared/either'
import {
  jsxSimpleAttributeToValue,
  getModifiableJSXAttributeAtPath,
} from '../core/shared/jsx-attributes'
import * as PP from '../core/shared/property-path'

export const AspectRatioPath = 'aspectRatio'
export const AspectRatioEnabledPath = `${AspectRatioPath}.enabled`

export function isAspectRatioLockedFromProps(props: any): boolean {
  return getValueOrDefaultFallbackForPath(AspectRatioEnabledPath, props)
}

export function isAspectRatioLockedNew(component: ElementInstanceMetadata): boolean {
  const element = component.element
  if (isRight(element) && isJSXElement(element.value)) {
    const props = element.value.props
    const aspectRatioProp = getModifiableJSXAttributeAtPath(
      props,
      PP.create(['data-aspect-ratio-locked']),
    )
    if (isLeft(aspectRatioProp)) {
      return false
    } else {
      return foldEither(
        () => false,
        (value) => (typeof value === 'boolean' ? value : false),
        jsxSimpleAttributeToValue(aspectRatioProp.value),
      )
    }
  } else {
    return isAspectRatioLockedFromProps(component.props)
  }
}
