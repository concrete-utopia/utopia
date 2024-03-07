import { foldEither, isLeft, isRight } from '../core/shared/either'
import type { ElementInstanceMetadata } from '../core/shared/element-template'
import { isJSXElement } from '../core/shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../core/shared/jsx-attribute-utils'
import * as PP from '../core/shared/property-path'
import * as ObjectPath from 'object-path'
import type { ElementProps } from './editor/store/editor-state'

export const AspectRatioPath = 'aspectRatio'
export const AspectRatioEnabledPath = `${AspectRatioPath}.enabled`

export const AspectRatioLockedProp = 'data-aspect-ratio-locked'

export function isAspectRatioLockedFromProps(props: any): boolean {
  return ObjectPath.get(props, AspectRatioEnabledPath) ?? false
}

export function isAspectRatioLockedNew(
  component: ElementInstanceMetadata,
  elementProps: ElementProps,
): boolean {
  const element = component.element
  if (isRight(element) && isJSXElement(element.value)) {
    const props = element.value.props
    const aspectRatioProp = getModifiableJSXAttributeAtPath(props, PP.create(AspectRatioLockedProp))
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
    return isAspectRatioLockedFromProps(elementProps)
  }
}
