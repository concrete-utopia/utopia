import type { JSXAttributes } from 'utopia-shared/src/types'
import * as Either from '../../../core/shared/either'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attribute-utils'
import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
import { cssParsers, type ParsedCSSProperties } from '../../inspector/common/css-utils'
import type { CSSStyleProperty, StyleInfo, UntypedStyleInfo } from '../canvas-types'
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

function getUntypedStyleInfo(jsxElementProps: JSXAttributes): UntypedStyleInfo | null {
  const styleProp = getJSXAttributesAtPath(jsxElementProps, PP.create('style'))
  if (
    styleProp.attribute.type === 'ATTRIBUTE_NOT_FOUND' ||
    styleProp.remainingPath != null ||
    styleProp.attribute.type === 'PART_OF_ATTRIBUTE_VALUE'
  ) {
    return null
  }

  if (
    styleProp.attribute.type === 'ATTRIBUTE_VALUE' &&
    typeof styleProp.attribute.value === 'object'
  ) {
    return styleProp.attribute.value
  }

  if (styleProp.attribute.type === 'ATTRIBUTE_NESTED_OBJECT') {
    let result: UntypedStyleInfo = {}
    styleProp.attribute.content.forEach((assignment) => {
      if (assignment.type === 'SPREAD_ASSIGNMENT') {
        return
      }

      if (typeof assignment.key !== 'string') {
        return
      }

      if (assignment.value.type !== 'ATTRIBUTE_VALUE') {
        result[assignment.key] = cssStylePropertyNotParsable(assignment.value)
        return
      }

      result[assignment.key] = cssStyleProperty(assignment.value.value, assignment.value)
    })
    return result
  }

  return null
}

function getPropertyFromInstance<P extends keyof StyleInfo, T = ParsedCSSProperties[P]>(
  prop: P,
  untypedStyleInfo: UntypedStyleInfo,
): CSSStyleProperty<NonNullable<T>> | null {
  const attribute = untypedStyleInfo[prop]
  if (attribute === undefined || attribute.type === 'not-found') {
    return cssStylePropertyNotFound()
  }
  if (attribute.type === 'not-parsable') {
    return attribute
  }
  const simpleValue = jsxSimpleAttributeToValue(attribute.propertyValue)
  if (Either.isLeft(simpleValue)) {
    return cssStylePropertyNotParsable(attribute.propertyValue)
  }
  const parser = cssParsers[prop] as (value: unknown) => Either.Either<string, T>
  const parsed = parser(simpleValue.value)
  if (Either.isLeft(parsed) || parsed.value == null) {
    return cssStylePropertyNotParsable(attribute.propertyValue)
  }
  return cssStyleProperty(parsed.value, attribute.propertyValue)
}

export const InlineStylePlugin: StylePlugin = {
  name: 'Inline Style',
  readUntypedStyleInfo: (projectContents, elementPath) => {
    const element = getJSXElementFromProjectContents(elementPath, projectContents)
    if (element == null) {
      return null
    }

    return getUntypedStyleInfo(element.props)
  },
  readStyleFromElementProps: <T extends keyof StyleInfo>(
    attributes: JSXAttributes,
    prop: T,
  ): CSSStyleProperty<NonNullable<ParsedCSSProperties[T]>> | null => {
    const untypedStyleInfo = getUntypedStyleInfo(attributes)
    if (untypedStyleInfo == null) {
      return null
    }
    return getPropertyFromInstance(prop, untypedStyleInfo)
  },
  styleInfoFactory:
    ({ projectContents }) =>
    (elementPath) => {
      const untypedStyleInfo = InlineStylePlugin.readUntypedStyleInfo(projectContents, elementPath)
      if (untypedStyleInfo == null) {
        return null
      }

      const gap = getPropertyFromInstance('gap', untypedStyleInfo)
      const flexDirection = getPropertyFromInstance('flexDirection', untypedStyleInfo)
      const left = getPropertyFromInstance('left', untypedStyleInfo)
      const right = getPropertyFromInstance('right', untypedStyleInfo)
      const top = getPropertyFromInstance('top', untypedStyleInfo)
      const bottom = getPropertyFromInstance('bottom', untypedStyleInfo)
      const width = getPropertyFromInstance('width', untypedStyleInfo)
      const height = getPropertyFromInstance('height', untypedStyleInfo)
      const flexBasis = getPropertyFromInstance('flexBasis', untypedStyleInfo)
      const padding = getPropertyFromInstance('padding', untypedStyleInfo)
      const paddingTop = getPropertyFromInstance('paddingTop', untypedStyleInfo)
      const paddingBottom = getPropertyFromInstance('paddingBottom', untypedStyleInfo)
      const paddingLeft = getPropertyFromInstance('paddingLeft', untypedStyleInfo)
      const paddingRight = getPropertyFromInstance('paddingRight', untypedStyleInfo)
      const borderRadius = getPropertyFromInstance('borderRadius', untypedStyleInfo)
      const borderTopLeftRadius = getPropertyFromInstance('borderTopLeftRadius', untypedStyleInfo)
      const borderTopRightRadius = getPropertyFromInstance('borderTopRightRadius', untypedStyleInfo)
      const borderBottomRightRadius = getPropertyFromInstance(
        'borderBottomRightRadius',
        untypedStyleInfo,
      )
      const borderBottomLeftRadius = getPropertyFromInstance(
        'borderBottomLeftRadius',
        untypedStyleInfo,
      )
      const zIndex = getPropertyFromInstance('zIndex', untypedStyleInfo)
      const flexWrap = getPropertyFromInstance('flexWrap', untypedStyleInfo)
      const overflow = getPropertyFromInstance('overflow', untypedStyleInfo)

      return {
        gap: gap,
        flexDirection: flexDirection,
        left: left,
        right: right,
        top: top,
        bottom: bottom,
        width: width,
        height: height,
        flexBasis: flexBasis,
        padding: padding,
        paddingTop: paddingTop,
        paddingBottom: paddingBottom,
        paddingLeft: paddingLeft,
        paddingRight: paddingRight,
        borderRadius: borderRadius,
        borderTopLeftRadius: borderTopLeftRadius,
        borderTopRightRadius: borderTopRightRadius,
        borderBottomRightRadius: borderBottomRightRadius,
        borderBottomLeftRadius: borderBottomLeftRadius,
        zIndex: zIndex,
        flexWrap: flexWrap,
        overflow: overflow,
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
