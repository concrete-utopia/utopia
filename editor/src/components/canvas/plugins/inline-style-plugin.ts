import type { JSXAttributes, PropertyPath } from 'utopia-shared/src/types'
import type { StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import * as Either from '../../../core/shared/either'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attribute-utils'
import type { ModifiableAttribute } from '../../../core/shared/jsx-attributes'
import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
import { cssParsers, type ParsedCSSProperties } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import type { CSSStyleProperty } from '../canvas-types'
import {
  cssStyleProperty,
  cssStylePropertyNotParsable,
  cssStylePropertyNotFound,
} from '../canvas-types'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'
import { applyValuesAtPath, deleteValuesAtPath } from '../commands/utils/property-utils'
import type { StylePlugin } from './style-plugins'

function getPropValue(attributes: JSXAttributes, path: PropertyPath): ModifiableAttribute {
  const result = getJSXAttributesAtPath(attributes, path)
  if (result.remainingPath != null) {
    return { type: 'ATTRIBUTE_NOT_FOUND' }
  }
  return result.attribute
}

function getPropertyFromInstance<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  prop: P,
  attributes: JSXAttributes,
): CSSStyleProperty<NonNullable<T>> | null {
  const attribute = getPropValue(attributes, stylePropPathMappingFn(prop, ['style']))
  if (attribute.type === 'ATTRIBUTE_NOT_FOUND') {
    return cssStylePropertyNotFound()
  }
  const simpleValue = jsxSimpleAttributeToValue(attribute)
  if (Either.isLeft(simpleValue)) {
    return cssStylePropertyNotParsable()
  }
  const parser = cssParsers[prop] as (value: unknown) => Either.Either<string, T>
  const parsed = parser(simpleValue.value)
  if (Either.isLeft(parsed) || parsed.value == null) {
    return cssStylePropertyNotParsable()
  }
  return cssStyleProperty(parsed.value)
}

export const InlineStylePlugin: StylePlugin = {
  name: 'Inline Style',
  styleInfoFactory:
    ({ projectContents }) =>
    (elementPath) => {
      const element = getJSXElementFromProjectContents(elementPath, projectContents)
      if (element == null) {
        return null
      }

      const gap = getPropertyFromInstance('gap', element.props)
      const flexDirection = getPropertyFromInstance('flexDirection', element.props)

      return {
        gap: gap,
        flexDirection: flexDirection,
      }
    },
  updateStyles: (editorState, elementPath, updates) => {
    const propsToDelete = mapDropNulls(
      (update) => (update.type === 'delete' ? PP.create('style', update.property) : null),
      updates,
    )

    const propsToSet = mapDropNulls(
      (update) =>
        update.type === 'set'
          ? {
              path: PP.create('style', update.property),
              value: jsExpressionValue(update.value, emptyComments),
            }
          : null,
      updates,
    )

    const { editorStateWithChanges: withValuesDeleted } = deleteValuesAtPath(
      editorState,
      elementPath,
      propsToDelete,
    )

    return applyValuesAtPath(withValuesDeleted, elementPath, propsToSet)
  },
}
