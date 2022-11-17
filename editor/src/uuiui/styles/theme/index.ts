import { colorTheme, ColorTheme, darkColorTheme } from './utopia-theme'
import { Theme } from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'

// TODO: don't export colorTheme anymore and just export useUtopiaTheme() hook
// prerequisites: no class components and usage of UtopiaTheme.color instead of colorTheme
export const useColorTheme = (): ColorTheme => {
  const currentTheme: Theme = useEditorState(
    (store) => store.userState.themeConfig ?? 'light',
    'currentTheme',
  )
  return currentTheme === 'dark' ? darkColorTheme : colorTheme
}

export { colorTheme, UtopiaTheme, UtopiaStyles } from './utopia-theme'
export type { ColorTheme } from './utopia-theme'
