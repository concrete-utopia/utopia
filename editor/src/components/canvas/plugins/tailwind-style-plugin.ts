import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { Right } from '../../../core/shared/either'
import { defaultEither, Either, flatMapEither, isLeft } from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import type { ParsedCSSProperties } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { StylePlugin, StylePluginContext } from './style-plugins'
import type { Config } from 'tailwindcss/types/config'
import type { ParsedVariant, StyleInfo } from '../canvas-types'
import { cssStyleProperty, cssVariant, type CSSStyleProperty } from '../canvas-types'
import * as UCL from './tailwind-style-plugin-utils/update-class-list'
import { assertNever } from '../../../core/shared/utils'
import {
  jsxSimpleAttributeToValue,
  getModifiableJSXAttributeAtPath,
} from '../../../core/shared/jsx-attribute-utils'
import { emptyComments, type JSXAttributes } from 'utopia-shared/src/types'
import * as PP from '../../../core/shared/property-path'
import { jsExpressionValue } from '../../../core/shared/element-template'
import { getModifiers } from './tailwind-style-plugin-utils/tailwind-responsive-utils'
import { getContainingSceneWidth, selectValueByBreakpoint } from '../responsive-utils'

type StyleValueVariants = {
  value: string | number | undefined
  modifiers?: TailwindGeneralModifier[]
}[]
export type TailwindMediaModifier = { type: 'media'; value: string }
export type TailwindHoverModifier = { type: 'hover'; value: string }
export type TailwindGeneralModifier = TailwindMediaModifier | TailwindHoverModifier

export const parseTailwindPropertyFactory =
  (config: Config | null, context: StylePluginContext) =>
  <T extends keyof StyleInfo>(
    styleDefinition: StyleValueVariants | undefined,
    prop: T,
  ): CSSStyleProperty<NonNullable<ParsedCSSProperties[T]>> | null => {
    if (styleDefinition == null) {
      return null
    }
    const possibleVariants: ParsedVariant<T>[] = styleDefinition
      .map((v) => ({
        parsedValue: cssParsers[prop](v.value, null),
        originalValue: v.value,
        modifiers: getModifiers(v.modifiers ?? [], config),
      }))
      .filter((v) => v.parsedValue != null && !isLeft(v.parsedValue))
      .map((v) => ({
        ...v,
        parsedValue: (v.parsedValue as Right<ParsedCSSProperties[T]>).value as NonNullable<
          ParsedCSSProperties[T]
        >,
      }))

    const selectedVariant = selectValueByBreakpoint(possibleVariants, context?.sceneWidth)
    if (selectedVariant == null) {
      return null
    }
    return cssStyleProperty(
      jsExpressionValue(selectedVariant.originalValue, emptyComments),
      cssVariant(selectedVariant.parsedValue, selectedVariant.modifiers),
      possibleVariants.map((variant) => cssVariant(variant.parsedValue, variant.modifiers)),
    )
  }

export const TailwindPropertyMapping: Record<string, string> = {
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
type TailwindParsedStyle = {
  kind: string
  property: string
  value: string
  variants?: { type: string; value: string }[]
}
export function getTailwindClassMapping(
  classes: string[],
  config: Config | null,
): Record<string, StyleValueVariants> {
  const mapping: Record<string, StyleValueVariants> = {}
  classes.forEach((className) => {
    const parsed: TailwindParsedStyle | undefined = TailwindClassParser.parse(
      className,
      config ?? undefined,
    )
    if (
      parsed == null ||
      parsed.kind === 'error' ||
      !isSupportedTailwindProperty(parsed.property)
    ) {
      return
    }
    mapping[parsed.property] = mapping[parsed.property] ?? []
    const modifiers = (parsed.variants ?? []).filter(
      (v): v is TailwindGeneralModifier => v.type === 'media' || v.type === 'hover',
    )
    mapping[parsed.property].push({
      value: parsed.value,
      modifiers: modifiers,
    })
  })
  return mapping
}

const underscoresToSpaces = (
  styleDef: StyleValueVariants | undefined,
): StyleValueVariants | undefined => {
  if (styleDef == null) {
    return undefined
  }
  return styleDef.map((style) => ({
    ...style,
    value: typeof style.value === 'string' ? style.value.replace(/[-_]/g, ' ') : style.value,
  }))
}

export const TailwindPlugin = (config: Config | null): StylePlugin => {
  return {
    name: 'Tailwind',
    readStyleFromElementProps: <P extends keyof StyleInfo>(
      attributes: JSXAttributes,
      prop: P,
      context: StylePluginContext,
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
      const parseTailwindProperty = parseTailwindPropertyFactory(config, context ?? {})
      return parseTailwindProperty(mapping[TailwindPropertyMapping[prop]], prop)
    },
    styleInfoFactory:
      ({ projectContents, jsxMetadata }) =>
      (elementPath) => {
        const classList = getClassNameAttribute(
          getElementFromProjectContents(elementPath, projectContents),
        )?.value

        if (classList == null || typeof classList !== 'string') {
          return null
        }

        const mapping = getTailwindClassMapping(classList.split(' '), config)
        const parseTailwindProperty = parseTailwindPropertyFactory(config, {
          sceneWidth: getContainingSceneWidth(elementPath, jsxMetadata),
        })
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
        { sceneWidth: getContainingSceneWidth(elementPath, editorState.jsxMetadata) },
      )
    },
  }
}
