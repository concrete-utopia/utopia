import { getDefaultProps, PropertyControls } from 'utopia-api'
import {
  emptyComments,
  JSXAttributes,
  jsxAttributesEntry,
  jsxAttributeValue,
} from '../shared/element-template'

export function getDefaultPropsAsAttributes(
  propertyControls: PropertyControls | null | undefined,
): JSXAttributes {
  const defaultProps = getDefaultProps(propertyControls ?? {})
  let result: JSXAttributes = []
  for (const key of Object.keys(defaultProps)) {
    result.push(
      jsxAttributesEntry(key, jsxAttributeValue(defaultProps[key], emptyComments), emptyComments),
    )
  }
  return result
}
