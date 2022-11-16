import { Theme } from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'
import {
  ColorTheme,
  colorTheme,
  colorThemeCssVariables,
  darkColorTheme,
  darkColorThemeCssVariables,
} from './utopia-theme'

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

export { colorTheme, UtopiaStyles, UtopiaTheme } from './utopia-theme'
export type { ColorTheme } from './utopia-theme'
// export { utopiaThemeCssVariables as UtopiaTheme }
// export { utopiaThemeCssValues }
