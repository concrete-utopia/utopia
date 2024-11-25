import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import { defaultEither, flatMapEither, isLeft } from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import type { ParsedCSSProperties } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { StylePlugin } from './style-plugins'
import type { Config } from 'tailwindcss/types/config'
import type { StyleInfo } from '../canvas-types'
import { cssStyleProperty, type CSSStyleProperty } from '../canvas-types'
import * as UCL from './tailwind-style-plugin-utils/update-class-list'
import { assertNever } from '../../../core/shared/utils'
import type { JSXAttributes } from 'utopia-shared/src/types'
import {
  jsxSimpleAttributeToValue,
  getModifiableJSXAttributeAtPath,
} from '../../../core/shared/jsx-attribute-utils'
import * as PP from '../../../core/shared/property-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'

const underscoresToSpaces = (s: string | undefined) => s?.replace(/[-_]/g, ' ')

function parseTailwindProperty<P extends keyof StyleInfo>(
  mapping: Record<string, string>,
  prop: P,
): CSSStyleProperty<NonNullable<ParsedCSSProperties[P]>> | null {
  const value = prop === 'padding' ? underscoresToSpaces(mapping[prop]) : mapping[prop]
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

  width: 'width',
  height: 'height',

  padding: 'padding',
  paddingTop: 'paddingTop',
  paddingRight: 'paddingRight',
  paddingBottom: 'paddingBottom',
  paddingLeft: 'paddingLeft',

  borderRadius: 'borderRadius',
  borderTopLeftRadius: 'borderTopLeftRadius',
  borderTopRightRadius: 'borderTopRightRadius',
  borderBottomRightRadius: 'borderBottomRightRadius',
  borderBottomLeftRadius: 'borderBottomLeftRadius',

  justifyContent: 'justifyContent',
  alignItems: 'alignItems',
  flex: 'flex',
  flexDirection: 'flexDirection',
  flexGrow: 'flexGrow',
  flexShrink: 'flexShrink',
  flexBasis: 'flexBasis',
  flexWrap: 'flexWrap',
  gap: 'gap',

  overflow: 'overflow',

  zIndex: 'zIndex',
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

function getTailwindClassMapping(classes: string[], config: Config | null): Record<string, string> {
  const mapping: Record<string, string> = {}
  classes.forEach((className) => {
    const parsed = TailwindClassParser.parse(className, config ?? undefined)
    if (parsed.kind === 'error') {
      return
    }
    parsed.valueDef.class.forEach((cls: string) => {
      mapping[toCamelCase(cls)] = parsed.value
    })
  })
  return mapping
}

export const TailwindPlugin = (config: Config | null): StylePlugin => ({
  name: 'Tailwind',
  readStyleFromElementProps: <P extends keyof StyleInfo>(
    attributes: JSXAttributes,
    prop: P,
  ): CSSStyleProperty<NonNullable<ParsedCSSProperties[P]>> | null => {
    const classNameAttribute = defaultEither(
      null,
      flatMapEither(
        (attr) => jsxSimpleAttributeToValue(attr),
        getModifiableJSXAttributeAtPath(attributes, PP.create('className')),
      ),
    )

    if (typeof classNameAttribute !== 'string') {
      return null
    }

    const mapping = getTailwindClassMapping(classNameAttribute.split(' '), config)
    return parseTailwindProperty(mapping, prop)
  },
  styleInfoFactory:
    ({ projectContents }) =>
    (elementPath) => {
      const classList = getClassNameAttribute(
        getElementFromProjectContents(elementPath, projectContents),
      )?.value

      if (classList == null || typeof classList !== 'string') {
        return null
      }

      const mapping = getTailwindClassMapping(classList.split(' '), config)

      return {
        gap: parseTailwindProperty(mapping, 'gap'),
        flexDirection: parseTailwindProperty(mapping, 'flexDirection'),
        left: parseTailwindProperty(mapping, 'left'),
        right: parseTailwindProperty(mapping, 'right'),
        top: parseTailwindProperty(mapping, 'top'),
        bottom: parseTailwindProperty(mapping, 'bottom'),
        width: parseTailwindProperty(mapping, 'width'),
        height: parseTailwindProperty(mapping, 'height'),
        flexBasis: parseTailwindProperty(mapping, 'flexBasis'),
        padding: parseTailwindProperty(mapping, 'padding'),
        paddingTop: parseTailwindProperty(mapping, 'paddingTop'),
        paddingRight: parseTailwindProperty(mapping, 'paddingRight'),
        paddingBottom: parseTailwindProperty(mapping, 'paddingBottom'),
        paddingLeft: parseTailwindProperty(mapping, 'paddingLeft'),
        zIndex: parseTailwindProperty(mapping, 'zIndex'),
        borderRadius: parseTailwindProperty(mapping, 'borderRadius'),
        borderTopLeftRadius: parseTailwindProperty(mapping, 'borderTopLeftRadius'),
        borderTopRightRadius: parseTailwindProperty(mapping, 'borderTopRightRadius'),
        borderBottomRightRadius: parseTailwindProperty(mapping, 'borderBottomRightRadius'),
        borderBottomLeftRadius: parseTailwindProperty(mapping, 'borderBottomLeftRadius'),
        flexWrap: parseTailwindProperty(mapping, 'flexWrap'),
        overflow: parseTailwindProperty(mapping, 'overflow'),
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

    const propsToSet = mapDropNulls(
      (update) =>
        update.type !== 'set' || TailwindPropertyMapping[update.property] == null
          ? null
          : UCL.add({
              property: TailwindPropertyMapping[update.property],
              value: stringifyPropertyValue(update.value),
            }),
      updates,
    )
    return UCL.runUpdateClassList(
      editorState,
      elementPath,
      [...propsToDelete, ...propsToSet],
      config,
    )
  },
})
