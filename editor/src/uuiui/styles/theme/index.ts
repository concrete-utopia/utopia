import type { ThemeObject } from './theme-helpers'
import { colorTheme } from './utopia-theme'

export type Theme = 'light' | 'dark'

export const useColorTheme = (): ThemeObject => {
  return colorTheme
}

export function getPreferredColorScheme(): Theme {
  if (window?.matchMedia?.('(prefers-color-scheme: dark)').matches) {
    return 'dark'
  } else {
    return 'light'
  }
}

export { colorTheme, UtopiaStyles, UtopiaTheme } from './utopia-theme'
