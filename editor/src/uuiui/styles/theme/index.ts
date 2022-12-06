import { Theme } from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'
import { ThemeObject } from './types'
import { colorTheme, colorThemeCssVariables, darkColorThemeCssVariables } from './utopia-theme'

export const useColorTheme = (): ThemeObject => {
  return colorTheme
}

export const useColorThemeVariables = (): any => {
  const currentTheme: Theme = useEditorState(
    (store) => store.userState.themeConfig ?? 'light',
    'currentTheme',
  )
  return currentTheme === 'dark' ? darkColorThemeCssVariables : colorThemeCssVariables
}

export { colorTheme, UtopiaStyles, UtopiaTheme } from './utopia-theme'
