import React, { useEffect, useState } from 'react'
import { getCurrentTheme } from '../../components/editor/store/editor-state'
import { useEditorState } from '../../components/editor/store/store-hook'
import { sendSetVSCodeTheme } from '../../core/vscode/vscode-bridge'
import { getPreferredColorScheme, Theme } from './theme'
import { mapToArray, objectFilter } from '../../core/shared/object-utils'
import {
  generateCssVariablesFromThemeObject,
  ThemeVariableObject,
  getIconColor,
  IcnColorOrNot,
} from './theme/theme-helpers'
import { dark } from './theme/dark'
import { light } from './theme/light'
import { mapDropNulls } from '../../core/shared/array-utils'

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
  const partialThemesObject = objectFilter<ThemeVariableObject>(
    (value) => typeof value !== 'string',
    themeVariables,
  )

  const partialThemes = mapDropNulls(
    (value) => (typeof value === 'string' ? null : value),
    mapToArray((value, key) => value, partialThemesObject),
  )

  return (
    <style>
      {/* Root variables based on the theme */}
      {':root {'}
      {mapToArray((value, variable) => {
        if (variable === 'iconColor') {
          if (IcnColorOrNot(value)) {
            return `${variable}:${getIconColor(value, currentTheme)}`
          } else {
            return `${variable}:main`
          }
        } else {
          return `${variable}:${value};`
        }
      }, mainThemeVars)}
      {'}'}
      {/* Classes with variables based on the subthemes */}
      {partialThemes.map((partialTheme) => {
        return (
          `.${partialTheme.name} {` +
          mapToArray((value, variable) => {
            if (variable === 'name') {
              return ''
            } else if (variable === 'iconColor') {
              if (IcnColorOrNot(value)) {
                return `${variable}:${getIconColor(value, currentTheme)}`
              } else {
                return `${variable}:main`
              }
            } else {
              return `${variable}:${value};`
            }
          }, partialTheme).join('\n') +
          `\n}`
        )
      }, partialThemes)}
    </style>
  )
})
