import React from 'react'
import { PartialThemeObject } from './theme/types'

interface ThemeProviderProps {
  theme?: PartialThemeObject
  active?: boolean
}
export const ThemeProvider = React.memo(
  ({ theme, active = true, children }: React.PropsWithChildren<ThemeProviderProps>) => {
    return (
      <div style={{ display: 'contents' }} className={active && theme != null ? theme.name : ''}>
        {children}
      </div>
    )
  },
)
