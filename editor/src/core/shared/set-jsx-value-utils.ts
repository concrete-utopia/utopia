import { getActivePluginSingleton } from '../../components/canvas/plugins/style-plugins'
import type { Either } from './either'
import type { JSXAttributes, JSExpression } from './element-template'
import { setJSXValueAtPathParts } from './jsx-attribute-utils'
import type { PropertyPath } from './project-file-types'

export function setJSXValueAtPathWithPlugin(
  attributes: JSXAttributes,
  path: PropertyPath,
  value: JSExpression,
): Either<string, JSXAttributes> {
  const valueForCSSProp =
    value.type !== 'ATTRIBUTE_VALUE' ||
    !(typeof value.value === 'number' || typeof value.value === 'string')
      ? null
      : value.value
  const [maybeStyle, maybeCSSProp] = path.propertyElements
  if (
    maybeStyle !== 'style' ||
    maybeCSSProp == null ||
    typeof maybeCSSProp !== 'string' ||
    valueForCSSProp == null
  ) {
    return setJSXValueAtPathParts(attributes, path.propertyElements, 0, value)
  }

  return getActivePluginSingleton().updateCSSPropertyInProps(attributes, [
    { type: 'set', property: maybeCSSProp, value: valueForCSSProp },
  ])
}
