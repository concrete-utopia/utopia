import { foldEither, isLeft, isRight } from '../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../core/shared/element-template'
import {
  GetModifiableAttributeResult,
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../core/shared/jsx-attributes'
import * as PP from '../core/shared/property-path'
import * as ObjectPath from 'object-path'
import { ElementProps } from './editor/store/editor-state'

export const AspectRatioPath = 'aspectRatio'
export const AspectRatioEnabledPath = `${AspectRatioPath}.enabled`

/**
 * @deprecated
 */
export const AspectRatioLockedProp = 'data-aspect-ratio-locked' as const // left here for compatibility with old projects
export const AspectRatioProp = 'aspectRatio' as const

function isAspectRatioLockedFromProps(props: ElementProps): boolean {
  return ObjectPath.get(props, AspectRatioEnabledPath) ?? false
}

function getBooleanProp(prop: GetModifiableAttributeResult) {
  if (isLeft(prop)) {
    return false
  } else {
    return foldEither(
      () => false,
      (value) => (typeof value === 'boolean' ? value : false),
      jsxSimpleAttributeToValue(prop.value),
    )
  }
}

export function isAspectRatioLockedNew(
  component: ElementInstanceMetadata,
  elementProps: ElementProps,
): boolean {
  const element = component.element
  if (isRight(element) && isJSXElement(element.value)) {
    const props = element.value.props
    const aspectRatioPropLegacy = getModifiableJSXAttributeAtPath(
      props,
      PP.create(AspectRatioLockedProp),
    )
    const aspectRatioProp = getModifiableJSXAttributeAtPath(
      props,
      PP.create('style', AspectRatioProp),
    )
    return getBooleanProp(aspectRatioProp) || getBooleanProp(aspectRatioPropLegacy)
  } else {
    return isAspectRatioLockedFromProps(elementProps)
  }
}
