import React, { useEffect, useState } from 'react'
import { getCurrentTheme } from '../../components/editor/store/editor-state'
import { useEditorState } from '../../components/editor/store/store-hook'
import { sendSetVSCodeTheme } from '../../core/vscode/vscode-bridge'
import { getPreferredColorScheme, Theme } from './theme'
import { mapToArray, objectFilter } from '../../core/shared/object-utils'
import { generateCssVariablesFromThemeObject, ThemeVariableObject } from './theme/theme-helpers'
import { dark } from './theme/dark'
import { light } from './theme/light'
import { mapDropNulls } from '../../core/shared/array-utils'
import { SubThemeObject } from './theme/types'

export const ColorThemeComponent = React.memo(() => {
  const themeSetting = useEditorState((store) => store.userState.themeConfig, 'currentTheme')
  const currentTheme: Theme = useEditorState(
    (store) => getCurrentTheme(store.userState),
    'currentTheme',
  )

  // a dummy state used to force a re-render when the system preferred color scheme
  // change event fires
  const [theme, setTheme] = useState(currentTheme)

  const colorTheme = currentTheme === 'dark' ? dark : light

  const themeVariables = generateCssVariablesFromThemeObject(colorTheme)

  useEffect(() => {
    const handlePreferredColorSchemeChange = () => {
      const preferredColorScheme = getPreferredColorScheme()
      void sendSetVSCodeTheme(preferredColorScheme)
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

  const mainThemeVars = objectFilter<ThemeVariableObject>(
    (value) => typeof value === 'string',
    themeVariables,
  )
  const subThemesObject = objectFilter<ThemeVariableObject>(
    (value) => typeof value !== 'string',
    themeVariables,
  )

  const subThemes = mapDropNulls(
    (value) => (typeof value === 'string' ? null : value),
    mapToArray((value, key) => value, subThemesObject),
  )

  return (
    <style>
      {/* Root variables based on the theme */}
      {':root {'}
      {mapToArray((value, variable) => {
        return `${variable}:${value};`
      }, mainThemeVars)}
      {'}'}
      {/* Classes with variables based on the subthemes */}
      {subThemes.map((subTheme) => {
        return (
          `.${subTheme.name} {` +
          mapToArray((value, variable) => {
            if (variable === 'name') {
              return ''
            } else {
              return `${variable}:${value};`
            }
          }, subTheme).join('\n') +
          `\n}`
        )
      }, subThemes)}
    </style>
  )
})

interface AlternateThemeProps {
  theme?: SubThemeObject
  active?: boolean
}

// TODO: Add context containing icon color, etc.
export const AltColorThemeProvider = React.memo(
  ({ theme, active = true, children }: React.PropsWithChildren<AlternateThemeProps>) => {
    return <div className={active && theme != null ? theme.name : ''}>{children}</div>
  },
)
