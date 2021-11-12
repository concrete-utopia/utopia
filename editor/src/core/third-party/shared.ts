import { ParseResult } from '../../utils/value-parser-utils'
import { ParsedPropertyControls } from '../property-controls/property-controls-parser'
import { getDefaultPropsFromParsedControls } from '../property-controls/property-controls-utils'
import {
  emptyComments,
  JSXAttributes,
  jsxAttributesEntry,
  jsxAttributeValue,
} from '../shared/element-template'

export function getDefaultPropsAsAttributesFromParsedControls(
  parsedControls: ParseResult<ParsedPropertyControls>,
): JSXAttributes {
  const defaultProps = getDefaultPropsFromParsedControls(parsedControls)
  let result: JSXAttributes = []
  for (const key of Object.keys(defaultProps)) {
    result.push(
      jsxAttributesEntry(key, jsxAttributeValue(defaultProps[key], emptyComments), emptyComments),
    )
  }
  return result
}
