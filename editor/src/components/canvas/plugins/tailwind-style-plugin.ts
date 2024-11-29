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
import {
  jsxSimpleAttributeToValue,
  getModifiableJSXAttributeAtPath,
} from '../../../core/shared/jsx-attribute-utils'
import { emptyComments, type JSXAttributes } from 'utopia-shared/src/types'
import * as PP from '../../../core/shared/property-path'
import { jsExpressionValue } from '../../../core/shared/element-template'
import { objectKeys } from '../../../core/shared/object-utils'

function parseTailwindProperty<T extends keyof StyleInfo>(
  value: string | number | undefined,
  prop: T,
): CSSStyleProperty<NonNullable<ParsedCSSProperties[T]>> | null {
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

const OrderedTailwindProperties: Record<keyof StyleInfo, number> = {
  gap: 1,
  flexDirection: 2,
  left: 3,
  right: 4,
  top: 5,
  bottom: 6,
  width: 7,
  height: 8,
  flexBasis: 9,
  padding: 10,
  paddingTop: 11,
  paddingRight: 12,
  paddingBottom: 13,
  paddingLeft: 14,
  borderRadius: 15,
  borderTopLeftRadius: 16,
  borderTopRightRadius: 17,
  borderBottomRightRadius: 18,
  borderBottomLeftRadius: 19,
  zIndex: 20,
  flexWrap: 21,
  overflow: 22,
}

function isSupportedTailwindProperty(prop: unknown): prop is keyof typeof TailwindPropertyMapping {
  return typeof prop === 'string' && prop in TailwindPropertyMapping
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
    if (parsed.kind === 'error' || !isSupportedTailwindProperty(parsed.property)) {
      return
    }
    mapping[parsed.property] = parsed.value
  })
  return mapping
}

const underscoresToSpaces = (s: string | undefined) => s?.replace(/[-_]/g, ' ')

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
    return parseTailwindProperty(mapping[TailwindPropertyMapping[prop]], prop)
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
        gap: parseTailwindProperty(mapping[TailwindPropertyMapping.gap], 'gap'),
        flexDirection: parseTailwindProperty(
          mapping[TailwindPropertyMapping.flexDirection],
          'flexDirection',
        ),
        left: parseTailwindProperty(mapping[TailwindPropertyMapping.left], 'left'),
        right: parseTailwindProperty(mapping[TailwindPropertyMapping.right], 'right'),
        top: parseTailwindProperty(mapping[TailwindPropertyMapping.top], 'top'),
        bottom: parseTailwindProperty(mapping[TailwindPropertyMapping.bottom], 'bottom'),
        width: parseTailwindProperty(mapping[TailwindPropertyMapping.width], 'width'),
        height: parseTailwindProperty(mapping[TailwindPropertyMapping.height], 'height'),
        flexBasis: parseTailwindProperty(mapping[TailwindPropertyMapping.flexBasis], 'flexBasis'),
        padding: parseTailwindProperty(
          underscoresToSpaces(mapping[TailwindPropertyMapping.padding]),
          'padding',
        ),
        paddingTop: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingTop],
          'paddingTop',
        ),
        paddingRight: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingRight],
          'paddingRight',
        ),
        paddingBottom: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingBottom],
          'paddingBottom',
        ),
        paddingLeft: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingLeft],
          'paddingLeft',
        ),
        borderRadius: parseTailwindProperty(
          mapping[TailwindPropertyMapping.borderRadius],
          'borderRadius',
        ),
        borderTopLeftRadius: parseTailwindProperty(
          mapping[TailwindPropertyMapping.borderTopLeftRadius],
          'borderTopLeftRadius',
        ),
        borderTopRightRadius: parseTailwindProperty(
          mapping[TailwindPropertyMapping.borderTopRightRadius],
          'borderTopRightRadius',
        ),
        borderBottomRightRadius: parseTailwindProperty(
          mapping[TailwindPropertyMapping.borderBottomRightRadius],
          'borderBottomRightRadius',
        ),
        borderBottomLeftRadius: parseTailwindProperty(
          mapping[TailwindPropertyMapping.borderBottomLeftRadius],
          'borderBottomLeftRadius',
        ),
        zIndex: parseTailwindProperty(mapping[TailwindPropertyMapping.zIndex], 'zIndex'),
        flexWrap: parseTailwindProperty(mapping[TailwindPropertyMapping.flexWrap], 'flexWrap'),
        overflow: parseTailwindProperty(mapping[TailwindPropertyMapping.overflow], 'overflow'),
      }
    },
  getOrderedStylePropertyKeys: (properties) => {
    return properties.sort((a, b) => OrderedTailwindProperties[a] - OrderedTailwindProperties[b])
  },
  updateStyles: (editorState, elementPath, updates) => {
    const propsToDelete = mapDropNulls(
      (update) =>
        update.type !== 'delete' || TailwindPropertyMapping[update.property] == null // TODO: make this type-safe
          ? null
          : UCL.remove(TailwindPropertyMapping[update.property]),
      updates,
    )

    const propsToSet = mapDropNulls(
      (update) =>
        update.type !== 'set' || TailwindPropertyMapping[update.property] == null // TODO: make this type-safe
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
