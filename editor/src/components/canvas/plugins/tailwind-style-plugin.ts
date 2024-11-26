import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
import type { ParsedCSSProperties } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { StylePlugin } from './style-plugins'
import type { Config } from 'tailwindcss/types/config'
import type { StyleInfo, UntypedStyleInfo } from '../canvas-types'
import { cssStyleProperty, type CSSStyleProperty } from '../canvas-types'
import * as UCL from './tailwind-style-plugin-utils/update-class-list'
import { assertNever } from '../../../core/shared/utils'
import type { JSXAttributes } from 'utopia-shared/src/types'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attribute-utils'
import * as PP from '../../../core/shared/property-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import { getParsedClassList } from '../../../core/tailwind/tailwind-class-list-utils'
import { isLeft } from '../../../core/shared/either'
import { textDecorationLine } from '../../../uuiui'

const underscoresToSpaces = (s: string | undefined) => s?.replace(/[-_]/g, ' ')

function parseTailwindProperty<P extends keyof StyleInfo>(
  mapping: UntypedStyleInfo,
  prop: P,
): CSSStyleProperty<NonNullable<ParsedCSSProperties[P]>> | null {
  const property = mapping[prop]
  if (property == null) {
    return null
  }
  if (property.type === 'not-parsable') {
    return property
  }

  const value =
    prop === 'padding' && typeof property.value === 'string'
      ? underscoresToSpaces(property.value)
      : property.value
  const parsed = cssParsers[prop](value, null)
  if (isLeft(parsed) || parsed.value == null) {
    return null
  }
  return cssStyleProperty(parsed.value, jsExpressionValue(value, emptyComments))
}

const TailwindPropertyMapping: Record<string, string> = {
  left: 'positionLeft',
  right: 'positionRight',
  top: 'positionTop',
  bottom: 'positionBottom',
  position: 'position',

  width: 'width',
  height: 'height',
  minWidth: 'minWidth',
  maxWidth: 'maxWidth',
  minHeight: 'minHeight',
  maxHeight: 'maxHeight',

  display: 'display',

  padding: 'padding',
  paddingTop: 'paddingTop',
  paddingRight: 'paddingRight',
  paddingBottom: 'paddingBottom',
  paddingLeft: 'paddingLeft',

  margin: 'margin',
  marginTop: 'marginTop',
  marginRight: 'marginRight',
  marginBottom: 'marginBottom',
  marginLeft: 'marginLeft',

  borderRadius: 'borderRadius',
  borderTopLeftRadius: 'borderTopLeftRadius',
  borderTopRightRadius: 'borderTopRightRadius',
  borderBottomRightRadius: 'borderBottomRightRadius',
  borderBottomLeftRadius: 'borderBottomLeftRadius',

  justifyContent: 'justifyContent',
  alignItems: 'alignItems',
  alignSelf: 'alignSelf',
  justifySelf: 'justifySelf',
  justifyItems: 'justifyItems',
  flex: 'flex',
  flexDirection: 'flexDirection',
  flexGrow: 'flexGrow',
  flexShrink: 'flexShrink',
  flexBasis: 'flexBasis',
  flexWrap: 'flexWrap',
  gap: 'gap',

  overflow: 'overflow',

  opacity: 'opacity',

  zIndex: 'zIndex',

  backgroundColor: 'backgroundColor',

  fontSize: 'fontSize',
  fontStyle: 'fontStyle',
  fontFamily: 'fontFamily',
  fontWeight: 'fontWeight',
  letterSpacing: 'letterSpacing',
  lineHeight: 'lineHeight',
  color: 'textColor',
  textDecoration: 'textDecoration',
  textDecorationColor: 'textDecorationColor',
  textDecorationStyle: 'textDecorationStyle',
  textDecorationLine: 'textDecoration',
  textAlign: 'textAlign',

  rowGap: 'gapX',
  columnGap: 'gapY',

  boxShadow: 'boxShadow',
}

function toCamelCase(str: string): string {
  return str
    .toLowerCase()
    .replace(/([-_][a-z])/g, (ltr) => ltr.toUpperCase())
    .replace(/[^a-zA-Z]/g, '')
}

function stringifyPropertyValue(value: string | number): string {
  switch (typeof value) {
    case 'number':
      return `${value}px`
    case 'string':
      return value
    default:
      assertNever(value)
  }
}

function getTailwindClassMapping(
  classNameAttribute: string,
  config: Config | null,
): Record<string, string> {
  const classes = getParsedClassList(classNameAttribute, config)
  const mapping: Record<string, string> = {}
  classes.forEach((className) => {
    if (className.type === 'unparsed') {
      return
    }
    className.ast.valueDef.class.forEach((cls: string) => {
      mapping[toCamelCase(cls)] = className.ast.valueDef.value
    })
  })
  return mapping
}

function getUntypedStyleInfoFromAttributes(
  attributes: JSXAttributes,
  config: Config | null,
): UntypedStyleInfo | null {
  const classNameAttribute = getModifiableJSXAttributeAtPath(attributes, PP.create('className'))
  if (
    classNameAttribute.type === 'LEFT' ||
    classNameAttribute.value.type !== 'ATTRIBUTE_VALUE' ||
    typeof classNameAttribute.value.value !== 'string'
  ) {
    return null
  }

  const mapping = getTailwindClassMapping(classNameAttribute.value.value, config)
  const result: UntypedStyleInfo = {}
  Object.entries(mapping).forEach(([key, value]) => {
    result[key] = cssStyleProperty(value, jsExpressionValue(value, emptyComments))
  })
  return result
}

export const TailwindPlugin = (config: Config | null): StylePlugin => ({
  name: 'Tailwind',
  readUntypedStyleInfo: (projectContents, elementPath) => {
    const element = getJSXElementFromProjectContents(elementPath, projectContents)
    if (element == null) {
      return null
    }
    return getUntypedStyleInfoFromAttributes(element.props, config)
  },
  readStyleFromElementProps: <P extends keyof StyleInfo>(
    attributes: JSXAttributes,
    prop: P,
  ): CSSStyleProperty<NonNullable<ParsedCSSProperties[P]>> | null => {
    const mapping = getUntypedStyleInfoFromAttributes(attributes, config)
    if (mapping == null) {
      return null
    }
    return parseTailwindProperty(mapping, prop)
  },
  styleInfoFactory:
    ({ projectContents }) =>
    (elementPath) => {
      const element = getJSXElementFromProjectContents(elementPath, projectContents)
      if (element == null) {
        return null
      }
      const untypedStyleInfo = getUntypedStyleInfoFromAttributes(element.props, config)
      if (untypedStyleInfo == null) {
        return null
      }

      return {
        gap: parseTailwindProperty(untypedStyleInfo, 'gap'),
        flexDirection: parseTailwindProperty(untypedStyleInfo, 'flexDirection'),
        left: parseTailwindProperty(untypedStyleInfo, 'left'),
        right: parseTailwindProperty(untypedStyleInfo, 'right'),
        top: parseTailwindProperty(untypedStyleInfo, 'top'),
        bottom: parseTailwindProperty(untypedStyleInfo, 'bottom'),
        width: parseTailwindProperty(untypedStyleInfo, 'width'),
        height: parseTailwindProperty(untypedStyleInfo, 'height'),
        flexBasis: parseTailwindProperty(untypedStyleInfo, 'flexBasis'),
        padding: parseTailwindProperty(untypedStyleInfo, 'padding'),
        paddingTop: parseTailwindProperty(untypedStyleInfo, 'paddingTop'),
        paddingRight: parseTailwindProperty(untypedStyleInfo, 'paddingRight'),
        paddingBottom: parseTailwindProperty(untypedStyleInfo, 'paddingBottom'),
        paddingLeft: parseTailwindProperty(untypedStyleInfo, 'paddingLeft'),
        zIndex: parseTailwindProperty(untypedStyleInfo, 'zIndex'),
        borderRadius: parseTailwindProperty(untypedStyleInfo, 'borderRadius'),
        borderTopLeftRadius: parseTailwindProperty(untypedStyleInfo, 'borderTopLeftRadius'),
        borderTopRightRadius: parseTailwindProperty(untypedStyleInfo, 'borderTopRightRadius'),
        borderBottomRightRadius: parseTailwindProperty(untypedStyleInfo, 'borderBottomRightRadius'),
        borderBottomLeftRadius: parseTailwindProperty(untypedStyleInfo, 'borderBottomLeftRadius'),
        flexWrap: parseTailwindProperty(untypedStyleInfo, 'flexWrap'),
        overflow: parseTailwindProperty(untypedStyleInfo, 'overflow'),
      }
    },
  updateStyles: (editorState, elementPath, updates) => {
    const propsToDelete = mapDropNulls(
      (update) =>
        update.type !== 'delete' || TailwindPropertyMapping[update.property] == null
          ? null
          : UCL.remove(TailwindPropertyMapping[update.property]),
      updates,
    )

    const propsToSet = mapDropNulls((update) => {
      return update.type !== 'set' || TailwindPropertyMapping[update.property] == null
        ? null
        : UCL.add({
            property: TailwindPropertyMapping[update.property],
            value: stringifyPropertyValue(update.value),
          })
    }, updates)
    return UCL.runUpdateClassList(
      editorState,
      elementPath,
      [...propsToDelete, ...propsToSet],
      config,
    )
  },
})
