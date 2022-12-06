import { useColorThemeVariables } from './theme'
import React from 'react'
import { mapToArray } from '../../core/shared/object-utils'

// TODO: Include alternate themes for the given primary theme
// and include CSS classes/variables for those
export const ColorThemeComponent = React.memo(() => {
  const themeVariables = useColorThemeVariables()

  return (
    <style>
      {':root {'}
      {mapToArray((value, variable) => {
        return `${variable}:${value};`
      }, themeVariables)}
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
