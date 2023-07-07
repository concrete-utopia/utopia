import React, { useEffect, useState } from 'react'
import { getCurrentTheme } from '../../components/editor/store/editor-state'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'
import { sendSetVSCodeTheme } from '../../core/vscode/vscode-bridge'
import type { Theme } from './theme'
import { getPreferredColorScheme } from './theme'
import { colorThemeCssVariables, darkColorThemeCssVariables } from './theme/utopia-theme'

export const ColorThemeComponent = React.memo(() => {
  const themeSetting = useEditorState(
    Substores.theme,
    (store) => store.userState.themeConfig,
    'currentTheme',
  )
  const currentTheme: Theme = useEditorState(
    Substores.theme,
    (store) => getCurrentTheme(store.userState),
    'currentTheme',
  )
  const colorTheme = currentTheme === 'dark' ? darkColorThemeCssVariables : colorThemeCssVariables

  // a dummy state used to force a re-render when the system preferred color scheme
  // change event fires
  const [theme, setTheme] = useState('')

  useEffect(() => {
    const handlePreferredColorSchemeChange = () => {
      const preferredColorScheme = getPreferredColorScheme()
      sendSetVSCodeTheme(preferredColorScheme)
      setTheme(preferredColorScheme)
    }

    const colorSchemeQuery = window?.matchMedia?.('(prefers-color-scheme: dark)')

    if (themeSetting === 'system') {
      colorSchemeQuery?.addEventListener('change', handlePreferredColorSchemeChange)
    }

    return function cleanup() {
      colorSchemeQuery?.removeEventListener('change', handlePreferredColorSchemeChange)
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
