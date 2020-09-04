import * as React from 'react'

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
          color: selected ? 'white' : '#888',
        }}
      >
        Use the operating system font: SF Pro on Apple devices, Roboto on Android, and Segoe UI on
        Windows
      </div>
    </div>
  )
}
