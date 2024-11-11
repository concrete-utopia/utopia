import type { JSXAttributes, PropertyPath } from 'utopia-shared/src/types'
import type { StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import type { PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as Either from '../../../core/shared/either'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'
import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
import { cssParsers, type ParsedCSSProperties } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import type { CSSStyleProperty, StyleInfo } from '../canvas-types'
import { applyValuesAtPath, deleteValuesAtPath } from '../commands/utils/property-utils'
import type { StylePlugin, StyleUpdate } from './style-plugins'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPathParts,
} from '../../../core/shared/jsx-attribute-utils'
import type { ModifiableAttribute } from '../../../core/shared/jsx-attributes'
import { unsetJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import Utils from '../../../utils/utils'
import { assertNever } from '../../../core/shared/utils'

function getPropValue(
  propsOrAttributes: PropsOrJSXAttributes,
  path: PropertyPath,
): ModifiableAttribute {
  if (Either.isLeft(propsOrAttributes)) {
    const possibleValue = Utils.path(PP.getElements(path), propsOrAttributes.value)
    if (typeof possibleValue === 'undefined') {
      return { type: 'ATTRIBUTE_NOT_FOUND' }
    }
    return jsExpressionValue(possibleValue, emptyComments)
  }

  if (Either.isRight(propsOrAttributes)) {
    const result = getJSXAttributesAtPath(propsOrAttributes.value, path)
    if (result.remainingPath != null) {
      return { type: 'ATTRIBUTE_NOT_FOUND' }
    }
    return result.attribute
  }

  assertNever(propsOrAttributes)
}

function getPropertyFromInstance<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  prop: P,
  propsOrAttributes: PropsOrJSXAttributes,
): CSSStyleProperty<NonNullable<T>> | null {
  const attribute = getPropValue(propsOrAttributes, stylePropPathMappingFn(prop, ['style']))
  if (attribute.type === 'ATTRIBUTE_NOT_FOUND') {
    return { type: 'not-found' }
  }
  const simpleValue = jsxSimpleAttributeToValue(attribute)
  if (Either.isLeft(simpleValue)) {
    return { type: 'not-editable' }
  }
  const parser = cssParsers[prop] as (value: unknown) => Either.Either<string, T>
  const parsed = parser(simpleValue.value)
  if (Either.isLeft(parsed) || parsed.value == null) {
    return { type: 'not-editable' }
  }
  return { type: 'property', tag: null, value: parsed.value }
}

export const InlineStylePlugin: StylePlugin = {
  name: 'Inline Style',
  readStyleFromElementProps: <T extends keyof StyleInfo>(
    propsOrAttributes: PropsOrJSXAttributes,
    key: T,
  ) => {
    return getPropertyFromInstance(key, propsOrAttributes) as StyleInfo[T]
  },
  styleInfoFactory:
    ({ projectContents }) =>
    (elementPath) => {
      const element = getJSXElementFromProjectContents(elementPath, projectContents)
      if (element == null) {
        return null
      }

      const gap = getPropertyFromInstance('gap', Either.right(element.props))
      const flexDirection = getPropertyFromInstance('flexDirection', Either.right(element.props))
      const padding = getPropertyFromInstance('padding', Either.right(element.props))
      const paddingTop = getPropertyFromInstance('paddingTop', Either.right(element.props))
      const paddingBottom = getPropertyFromInstance('paddingBottom', Either.right(element.props))
      const paddingLeft = getPropertyFromInstance('paddingLeft', Either.right(element.props))
      const paddingRight = getPropertyFromInstance('paddingRight', Either.right(element.props))
      const width = getPropertyFromInstance('width', Either.right(element.props))
      const height = getPropertyFromInstance('height', Either.right(element.props))
      const top = getPropertyFromInstance('top', Either.right(element.props))
      const left = getPropertyFromInstance('left', Either.right(element.props))
      const right = getPropertyFromInstance('right', Either.right(element.props))
      const bottom = getPropertyFromInstance('bottom', Either.right(element.props))
      const flexBasis = getPropertyFromInstance('flexBasis', Either.right(element.props))
      const flexGrow = getPropertyFromInstance('flexGrow', Either.right(element.props))
      const flex = getPropertyFromInstance('flex', Either.right(element.props))

      return {
        gap,
        flexDirection,
        flexBasis,
        padding,
        paddingTop,
        paddingBottom,
        paddingLeft,
        paddingRight,
        width,
        height,
        top,
        left,
        right,
        bottom,
        flexGrow,
        flex,
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
  updateCSSPropertyInProps: (
    props: JSXAttributes,
    updates: StyleUpdate[],
  ): PropsOrJSXAttributes => {
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

    const withPropsUnset = propsToDelete.reduce(
      (acc, propertyPath) => Either.defaultEither(acc, unsetJSXValueAtPath(acc, propertyPath)),
      props,
    )
    const withPropsSet = propsToSet.reduce(
      (acc, { path, value }) =>
        Either.defaultEither(acc, setJSXValueAtPathParts(props, PP.getElements(path), 0, value)),
      withPropsUnset,
    )

    return Either.right(withPropsSet)
  },
}
