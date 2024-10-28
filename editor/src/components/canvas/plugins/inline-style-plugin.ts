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
import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
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
    ({ projectContents }) =>
    (elementPath) => {
      const element = getJSXElementFromProjectContents(elementPath, projectContents)
      if (element == null) {
        return null
      }

      const gap = getPropertyFromInstance('gap', element)
      const flexDirection = getPropertyFromInstance('flexDirection', element)
      const padding = getPropertyFromInstance('padding', element)
      const paddingTop = getPropertyFromInstance('paddingTop', element)
      const paddingBottom = getPropertyFromInstance('paddingBottom', element)
      const paddingLeft = getPropertyFromInstance('paddingLeft', element)
      const paddingRight = getPropertyFromInstance('paddingRight', element)
      const width = getPropertyFromInstance('width', element)
      const height = getPropertyFromInstance('height', element)
      const top = getPropertyFromInstance('top', element)
      const left = getPropertyFromInstance('left', element)
      const right = getPropertyFromInstance('right', element)
      const bottom = getPropertyFromInstance('bottom', element)
      const flexBasis = getPropertyFromInstance('flexBasis', element)

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
