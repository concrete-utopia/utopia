import * as React from 'react'
import { UtopiaTheme } from '../../../../../uuiui'

interface SystedDefaultFontItemProps {
  selected: boolean
}

export const SystedDefaultFontItem: React.FunctionComponent<SystedDefaultFontItemProps> = ({
  selected,
}) => {
  return (
    <div>
      <div>System Default</div>
      <div
        style={{
          fontSize: 11,
          whiteSpace: 'normal',
          backgroundColor: selected ? UtopiaTheme.color.inspectorFocusedColor.value : undefined,
          color: selected ? 'white' : '#888',
        }}
      >
        Use the default typeface of the operating system: SF Pro on macOS and iOS, Roboto on Android
        and Segoe UI on Windows
      </div>
    </div>
  )
}
