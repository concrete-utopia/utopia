import { getDefaultProps, PropertyControls } from 'utopia-api'
import { JSXAttributes, jsxAttributesEntry, jsxAttributeValue } from '../shared/element-template'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'

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
