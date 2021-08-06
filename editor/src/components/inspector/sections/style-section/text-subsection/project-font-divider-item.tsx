import * as React from 'react'
import { useColorTheme } from '../../../../../uuiui'

export const ProjectFontDividerItem: React.FunctionComponent = () => {
  const colorTheme = useColorTheme()
  return (
    <div style={{ width: '100%', height: 1, backgroundColor: colorTheme.neutralBorder.value }} />
  )
}
