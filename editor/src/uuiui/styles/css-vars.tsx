import { useColorThemeVariables } from './theme'
import React from 'react'

export const ColorThemeStyleComponent = React.memo(() => {
  const colorTheme = useColorThemeVariables(),
    { inverted, ...currentTheme } = colorTheme

  return (
    <style>
      {':root {'}
      {Object.entries({ ...inverted, ...currentTheme }).map(
        ([variable, value]) => `${variable}:${value};`,
      )}
      {'}'}
    </style>
  )
})
