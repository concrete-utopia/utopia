import { useColorThemeAlternateVariables, useColorThemeVariables } from './theme'
import React from 'react'

export const ColorThemeComponent = React.memo(() => {
  const colorTheme = useColorThemeVariables()
  const alternateColorTheme = useColorThemeAlternateVariables()

  return (
    <style>
      {':root {'}
      {Object.entries(colorTheme).map(([variable, value]) => {
        return `${variable}:${value};`
      })}
      {'}'}

      {'.utopitheme-alternate-colors {'}
      {Object.entries(alternateColorTheme).map(([variable, value]) => {
        return `${variable}:${value};`
      })}
      {'}'}
    </style>
  )
})

interface AlternateThemeProps {}

export const AlternateColorThemeComponent = React.memo(
  (props: React.PropsWithChildren<AlternateThemeProps>) => {
    return <div className='utopitheme-alternate-colors'>{props.children}</div>
  },
)
