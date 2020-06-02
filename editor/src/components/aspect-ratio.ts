import { foldEither, isLeft, isRight } from '../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../core/shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../core/shared/jsx-attributes'
import * as PP from '../core/shared/property-path'
import * as ObjectPath from 'object-path'

export const AspectRatioPath = 'aspectRatio'
export const AspectRatioEnabledPath = `${AspectRatioPath}.enabled`

export function isAspectRatioLockedFromProps(props: any): boolean {
  return ObjectPath.get(props, AspectRatioEnabledPath) ?? false
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
