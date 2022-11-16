import { Theme } from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'
import { dark } from './dark'
import { light } from './light'
import { ColorTheme, colorTheme, darkColorTheme, darkColorThemeCssVariables } from './utopia-theme'
import type { UtopiColor } from '../utopi-color-helpers'

export const colorThemeCssVariables = Object.keys(light).reduce((prev, curr) => {
  const parts = curr.replace(/([A-Z]|[0-9]+)/g, '-$1'),
    varName = `--${parts.toLowerCase()}`
  return { ...prev, [curr]: varName }
}, {})

// TODO: don't export colorTheme anymore and just export useUtopiaTheme() hook
// prerequisites: no class components and usage of UtopiaTheme.color instead of colorTheme
export const useColorTheme = (): ColorTheme => {
  const currentTheme: Theme = useEditorState((store) => store.userState.themeConfig, 'currentTheme')
  return currentTheme === 'dark' ? darkColorTheme : colorTheme
}
export const useColorThemeVariables = (): any => {
  const currentTheme: Theme = useEditorState((store) => store.userState.themeConfig, 'currentTheme')
  return currentTheme === 'dark' ? darkColorThemeCssVariables : colorThemeCssVariables
}

export { UtopiaTheme, UtopiaStyles, colorTheme } from './utopia-theme'
export type { ColorTheme } from './utopia-theme'
// export { utopiaThemeCssVariables as UtopiaTheme }
// export { utopiaThemeCssValues }
