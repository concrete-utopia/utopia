// TODO: pass in colorTheme to updateCssVars from a hook and remove the dependency here:
import {
  colorThemeCssVariables,
  useColorTheme,
  useColorThemeVariables,
  UtopiaTheme,
  // utopiaThemeCssValues
} from './theme'
import React from 'react'

export const ColorThemeStyleComponent = React.memo(() => {
  const colorTheme = useColorThemeVariables(),
    { inverted, ...currentTheme } = colorTheme

  // console.log(colorTheme)

  return (
    <style>
      {':root {'}
      {Object.entries(colorThemeCssVariables).map((entry) => {
        const [key, variable] = entry
        const cssValue = currentTheme[key as keyof typeof currentTheme].cssValue
        return `${variable}:${cssValue};`
      })}
      {/* {Object.entries(utopiaThemeCssValues).map((entry) => {
        const [variable, value] = entry
        return `${variable}:${value};`
      })} */}
      {'}'}
    </style>
  )
})
