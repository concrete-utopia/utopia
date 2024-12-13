import type { Config } from 'tailwindcss/types/config'
import { type StyleMediaSizeModifier, type StyleModifier } from '../../canvas-types'
import type { ScreenSize } from '../../responsive-types'
import { extractScreenSizeFromCss } from '../../responsive-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'

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
    mapDropNulls(([key, size]) => {
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
    }, Object.entries(screenSizes)),
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
