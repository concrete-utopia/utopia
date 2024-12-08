import type { Config } from 'tailwindcss/types/config'
import {
  isStyleInfoKey,
  type ScreenSize,
  type StyleMediaSizeModifier,
  type StyleModifier,
} from '../../canvas-types'
import { extractScreenSizeFromCss } from '../../canvas-utils'
import { TailwindPropertyMapping } from '../tailwind-style-plugin'
import { parseTailwindPropertyFactory } from '../tailwind-style-plugin'
import { getTailwindClassMapping } from '../tailwind-style-plugin'

export const TAILWIND_DEFAULT_SCREENS = {
  sm: '640px',
  md: '768px',
  lg: '1024px',
  xl: '1280px',
  '2xl': '1536px',
}
const defaultTailwindConfig = {
  theme: {
    screens: TAILWIND_DEFAULT_SCREENS,
  },
} as unknown as Config
type TailwindScreen = string | { min: string; max: string }

export function screensConfigToScreenSizes(config: Config | null): Record<string, ScreenSize> {
  const tailwindConfig = config ?? defaultTailwindConfig
  const screenSizes: Record<string, TailwindScreen> = {
    ...((tailwindConfig.theme?.screens as Record<string, TailwindScreen>) ??
      TAILWIND_DEFAULT_SCREENS),
    ...((tailwindConfig.theme?.extend?.screens as Record<string, TailwindScreen>) ?? {}),
  }

  return Object.fromEntries(
    Object.entries(screenSizes)
      .map(([key, size]) => {
        const mediaString =
          typeof size === 'string'
            ? `@media (min-width: ${size})`
            : `@media ${[
                size.min != null ? `(min-width: ${size.min})` : '',
                size.max != null ? `(max-width: ${size.max})` : '',
              ]
                .filter((s) => s != '')
                .join(' and ')}`

        const screenSize = extractScreenSizeFromCss(mediaString)
        if (screenSize == null) {
          return null
        }
        return [key, screenSize]
      })
      .filter((entry): entry is [string, ScreenSize] => entry != null),
  )
}

/**
 * This function gets variants in the form of {type: 'media', value: 'sm'}
 * and turns them into modifiers in the form of [{type: 'media-size', size: {min: {value: 0, unit: 'px'}, max: {value: 100, unit: 'em'}}}]
 * according to the tailwind config
 */
export function getModifiers(
  variants: { type: string; value: string }[],
  config: Config | null,
): StyleModifier[] {
  const mediaModifiers = variants.filter((v) => v.type === 'media')
  const screenSizes = screensConfigToScreenSizes(config)

  return mediaModifiers
    .map((mediaModifier) => {
      const size = screenSizes[mediaModifier.value]
      if (size == null) {
        return null
      }
      return {
        type: 'media-size',
        size: size,
        modifierOrigin: { type: 'tailwind', variant: mediaModifier.value },
      } as StyleMediaSizeModifier
    })
    .filter((m): m is StyleMediaSizeModifier => m != null)
}

export function getPropertiesToAppliedModifiersMap(
  currentClassNameAttribute: string,
  propertyNames: string[],
  config: Config | null,
  context: {
    sceneWidth?: number
  },
): Record<string, StyleModifier[]> {
  const parseTailwindProperty = parseTailwindPropertyFactory(config, context)
  const classMapping = getTailwindClassMapping(currentClassNameAttribute.split(' '), config)
  return propertyNames.reduce((acc, propertyName) => {
    if (!isStyleInfoKey(propertyName)) {
      return acc
    }
    const parsedProperty = parseTailwindProperty(
      classMapping[TailwindPropertyMapping[propertyName]],
      propertyName,
    )
    if (parsedProperty?.type == 'property' && parsedProperty.appliedModifiers != null) {
      return {
        ...acc,
        [propertyName]: parsedProperty.appliedModifiers,
      }
    } else {
      return acc
    }
  }, {} as Record<string, StyleModifier[]>)
}

export function getTailwindVariantFromAppliedModifier(
  appliedModifier: StyleMediaSizeModifier | null,
): string | null {
  return appliedModifier?.modifierOrigin?.type === 'tailwind'
    ? appliedModifier.modifierOrigin.variant
    : null
}
