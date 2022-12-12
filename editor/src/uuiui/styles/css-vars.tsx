import { useColorThemeVariables } from './theme'
import React from 'react'

// TODO: Include alternate themes for the given primary theme
// and include CSS classes/variables for those
export const ColorThemeComponent = React.memo(() => {
  const colorTheme = useColorThemeVariables()

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
