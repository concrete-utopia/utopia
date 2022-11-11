import { UtopiColor } from './utopi-color-helpers'
// TODO: pass in colorTheme to updateCssVars from a hook and remove the dependency here:
import { cssVariables, useColorTheme } from './theme'
import React from 'react'

export const StyleComponent = React.memo(() => {
  const colorTheme = useColorTheme(),
    { inverted, ...currentTheme } = colorTheme

  return (
    <style>
      {':root {'}
      {Object.entries(cssVariables).map((entry) => {
        const [key, variable] = entry
        const cssValue = currentTheme[key as keyof typeof currentTheme].cssValue
        return `${variable}:${cssValue};`
      })}
      {'--theme-setting: light'}
      {'}'}
    </style>
  )
})
