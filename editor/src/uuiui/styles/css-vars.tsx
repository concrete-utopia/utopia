import React, { useEffect, useState } from 'react'
import { getCurrentTheme } from '../../components/editor/store/editor-state'
import { useEditorState } from '../../components/editor/store/store-hook'
import { sendSetVSCodeTheme } from '../../core/vscode/vscode-bridge'
import { getPreferredColorScheme, Theme } from './theme'
import { colorThemeCssVariables, darkColorThemeCssVariables } from './theme/utopia-theme'

export const useColorThemeVariables = (): any => {
  const currentTheme: Theme = useEditorState(
    (store) => getCurrentTheme(store.userState),
    'currentTheme',
  )

  return currentTheme === 'dark' ? darkColorThemeCssVariables : colorThemeCssVariables
}

export const useColorThemeAlternateVariables = (): any => {
  const currentTheme: Theme = useEditorState(
    (store) => getCurrentTheme(store.userState),
    'currentTheme',
  )
  return currentTheme === 'light' ? darkColorThemeCssVariables : colorThemeCssVariables
}

// TODO: Include alternate themes for the given primary theme
// and include CSS classes/variables for those
export const ColorThemeComponent = React.memo(() => {
  const themeSetting = useEditorState((store) => store.userState.themeConfig, 'currentTheme')
  const colorTheme = useColorThemeVariables()

  // a dummy state used to force a re-render when the system preferred color scheme
  // change event fires
  const [theme, setTheme] = useState('')

  useEffect(() => {
    const handlePreferredColorSchemeChange = () => {
      const preferredColorScheme = getPreferredColorScheme()
      void sendSetVSCodeTheme(preferredColorScheme)
      setTheme(preferredColorScheme)
    }

    if (themeSetting === 'system') {
      const colorSchemeQuery = window.matchMedia('(prefers-color-scheme: dark)')
      colorSchemeQuery.addEventListener('change', handlePreferredColorSchemeChange)
      return () => colorSchemeQuery.removeEventListener('change', handlePreferredColorSchemeChange)
    } else {
      return
    }
  }, [themeSetting, theme])

  return (
    <style>
      {':root {'}
      {Object.entries(colorTheme).map(([variable, value]) => {
        return `${variable}:${value};`
      })}
      {'}'}
    </style>
  )
})

interface AlternateThemeProps {}

// TODO: Extend this to work with multiple alternate "themes"
export const AlternateColorThemeComponent = React.memo(
  (props: React.PropsWithChildren<AlternateThemeProps>) => {
    return <div className='utopitheme-alternate-colors'>{props.children}</div>
  },
)
