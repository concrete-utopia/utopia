import React from 'react'
import { useColorTheme } from '../../../../../uuiui'

interface SystedDefaultFontItemProps {
  selected: boolean
}

export const SystedDefaultFontItem: React.FunctionComponent<
  React.PropsWithChildren<SystedDefaultFontItemProps>
> = ({ selected }) => {
  const colorTheme = useColorTheme()
  return (
    <div>
      <div>System Default</div>
      <div
        style={{
          fontSize: 11,
          whiteSpace: 'normal',
          color: selected
            ? colorTheme.neutralInvertedForeground.value
            : colorTheme.subduedForeground.value,
        }}
      >
        Use the operating system font: SF Pro on Apple devices, Roboto on Android, and Segoe UI on
        Windows
      </div>
    </div>
  )
}
