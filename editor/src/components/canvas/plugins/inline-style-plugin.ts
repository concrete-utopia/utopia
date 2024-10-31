import type { PropertyPath } from 'utopia-shared/src/types'
import type { StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import type { PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as Either from '../../../core/shared/either'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'
import { Utils } from '../../../uuiui-deps'
import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
import { cssParsers, type ParsedCSSProperties } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import type { CSSStyleProperty, StyleInfo } from '../canvas-types'
import { applyValuesAtPath, deleteValuesAtPath } from '../commands/utils/property-utils'
import type { StylePlugin } from './style-plugins'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attribute-utils'

function getPropValue(
  propsOrAttributes: PropsOrJSXAttributes,
  path: PropertyPath,
): Either.Either<string, any> {
  return Either.foldEither(
    (props) => {
      const possibleValue = Utils.path(PP.getElements(path), props)
      if (possibleValue == null) {
        return Either.right(undefined)
      } else {
        return Either.right(possibleValue)
      }
    },
    (attributes) => {
      const getAttrResult = getModifiableJSXAttributeAtPath(attributes, path)
      return Either.flatMapEither((attr) => jsxSimpleAttributeToValue(attr), getAttrResult)
    },
    propsOrAttributes,
  )
}

function getPropertyFromInstance<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  prop: P,
  propsOrAttributes: PropsOrJSXAttributes,
): CSSStyleProperty<NonNullable<T>> | null {
  const attribute = getPropValue(propsOrAttributes, stylePropPathMappingFn(prop, ['style']))
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
}
