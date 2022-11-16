// TODO: pass in colorTheme to updateCssVars from a hook and remove the dependency here:
import {
  useColorThemeVariables,
  // utopiaThemeCssValues
} from './theme'
import React from 'react'

export const ColorThemeStyleComponent = React.memo(() => {
  const colorTheme = useColorThemeVariables(),
    { inverted, ...currentTheme } = colorTheme

  return (
    <style>
      {':root {'}
      {Object.entries({ ...inverted, ...currentTheme }).map((entry) => {
        const [variable, value] = entry
        return `${variable}:${value};`
      })}
      {/* {Object.entries(utopiaThemeCssValues).map((entry) => {
        const [variable, value] = entry
        return `${variable}:${value};`
      })} */}
      {'}'}
    </style>
  )
})
