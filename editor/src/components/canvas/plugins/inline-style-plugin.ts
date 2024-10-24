import type { StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as Either from '../../../core/shared/either'
import type { JSXElement } from '../../../core/shared/element-template'
import {
  emptyComments,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'
import { cssParsers, type ParsedCSSProperties } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import type { CSSStyleProperty } from '../canvas-types'
import { applyValuesAtPath, deleteValuesAtPath } from '../commands/utils/property-utils'
import type { StylePlugin } from './style-plugins'

function getPropertyFromInstance<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  prop: P,
  element: JSXElement,
): CSSStyleProperty<NonNullable<T>> | null {
  const attribute = getSimpleAttributeAtPath(
    Either.right(element.props),
    stylePropPathMappingFn(prop, ['style']),
  )
  if (Either.isLeft(attribute) || attribute.value == null) {
    return { type: 'not-found' }
  }
  const parser = cssParsers[prop] as (value: unknown) => Either.Either<string, T>
  const parsed = parser(attribute.value)
  if (Either.isLeft(parsed) || parsed.value == null) {
    return { type: 'not-editable' }
  }
  return { type: 'property', tag: null, value: parsed.value }
}

export const InlineStylePlugin: StylePlugin = {
  name: 'Inline Style',
  styleInfoFactory:
    ({ metadata }) =>
    (elementPath) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
      if (
        instance == null ||
        Either.isLeft(instance.element) ||
        !isJSXElement(instance.element.value)
      ) {
        return null
      }

      const gap = getPropertyFromInstance('gap', instance.element.value)
      const flexDirection = getPropertyFromInstance('flexDirection', instance.element.value)
      const padding = getPropertyFromInstance('padding', instance.element.value)
      const paddingTop = getPropertyFromInstance('paddingTop', instance.element.value)
      const paddingBottom = getPropertyFromInstance('paddingBottom', instance.element.value)
      const paddingLeft = getPropertyFromInstance('paddingLeft', instance.element.value)
      const paddingRight = getPropertyFromInstance('paddingRight', instance.element.value)
      const width = getPropertyFromInstance('width', instance.element.value)
      const height = getPropertyFromInstance('height', instance.element.value)
      const top = getPropertyFromInstance('top', instance.element.value)
      const left = getPropertyFromInstance('left', instance.element.value)
      const right = getPropertyFromInstance('right', instance.element.value)
      const bottom = getPropertyFromInstance('bottom', instance.element.value)

      return {
        gap,
        flexDirection,
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
