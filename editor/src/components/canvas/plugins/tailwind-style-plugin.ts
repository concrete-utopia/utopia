import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { Right } from '../../../core/shared/either'
import { defaultEither, Either, flatMapEither, isLeft } from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import type { ParsedCSSProperties } from '../../inspector/common/css-utils'
import { cssParsers, selectValueByBreakpoint } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { StylePlugin } from './style-plugins'
import type { Config } from 'tailwindcss/types/config'
import type {
  ParsedVariant,
  StyleInfo,
  StyleMediaSizeModifier,
  StyleModifier,
} from '../canvas-types'
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
import { extractMediaQueryFromCss, mediaQueryToScreenSize } from '../canvas-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

type StyleValueVariants = {
  value: string | number | undefined
  modifiers?: TailwindGeneralModifier[]
}[]
export type TailwindMediaModifier = { type: 'media'; value: string }
export type TailwindHoverModifier = { type: 'hover'; value: string }
export type TailwindGeneralModifier = TailwindMediaModifier | TailwindHoverModifier

const parseTailwindPropertyFactory =
  (
    config: Config | null,
    context: {
      sceneWidth?: number
    },
  ) =>
  <T extends keyof StyleInfo>(
    styleDefinition: StyleValueVariants | undefined,
    prop: T,
  ): CSSStyleProperty<NonNullable<ParsedCSSProperties[T]>> | null => {
    if (styleDefinition == null) {
      return null
    }
    const parsed: ParsedVariant<T>[] = styleDefinition
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

    const result = selectValueByBreakpoint(parsed, context?.sceneWidth)
    if (result == null) {
      return null
    }
    return cssStyleProperty(
      result.parsedValue,
      jsExpressionValue(result.originalValue, emptyComments),
      result.modifiers,
      parsed.map((variant) => ({
        value: variant.parsedValue,
        modifiers: variant.modifiers,
      })),
    )
  }

type TailwindScreen = string | { min: string; max: string }

/**
 * This function gets variants in the form of {type: 'media', value: 'sm'}
 * and turns them into modifiers in the form of [{type: 'media-size', size: {min: {value: 0, unit: 'px'}, max: {value: 100, unit: 'em'}}}]
 * according to the tailwind config
 */
function getModifiers(
  variants: { type: string; value: string }[],
  config: Config | null,
): StyleModifier[] {
  // media modifiers
  const mediaModifiers = variants.filter((v) => v.type === 'media')
  const screenSizes: Record<string, TailwindScreen> = (config?.theme?.screens as Record<
    string,
    TailwindScreen
  >) ?? {
    sm: '640px',
    md: '768px',
    lg: '1024px',
    xl: '1280px',
    '2xl': '1536px',
    ...((config?.theme?.extend?.screens as Record<string, TailwindScreen>) ?? {}),
  }
  return mediaModifiers
    .map((mediaModifier) => {
      const size: string | { min: string; max: string } | undefined =
        screenSizes[mediaModifier.value as keyof typeof screenSizes]
      if (size == null) {
        return null
      }
      const mediaString =
        typeof size === 'string'
          ? `@media (min-width: ${size})`
          : `@media ${[
              size.min != null ? `(min-width: ${size.min})` : '',
              size.max != null ? `(max-width: ${size.max})` : '',
            ]
              .filter((s) => s != '')
              .join(' and ')}`
      const parsed = extractMediaQueryFromCss(mediaString)
      if (parsed == null) {
        return null
      }
      const asScreenSize = mediaQueryToScreenSize(parsed)
      if (asScreenSize == null) {
        return null
      }
      return {
        type: 'media-size',
        size: asScreenSize,
        modifierOrigin: { type: 'tailwind', variant: mediaModifier.value },
      } as StyleMediaSizeModifier
    })
    .filter((m): m is StyleMediaSizeModifier => m != null)
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
function getTailwindClassMapping(
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
      context?: {
        sceneWidth?: number
      },
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
        const containingScene = MetadataUtils.getParentSceneMetadata(jsxMetadata, elementPath)
        const parseTailwindProperty = parseTailwindPropertyFactory(config, {
          sceneWidth: containingScene?.specialSizeMeasurements?.clientWidth,
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
      )
    },
  }
}
