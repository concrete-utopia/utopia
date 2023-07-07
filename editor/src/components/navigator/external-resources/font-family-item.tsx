import React from 'react'
import { FlexRow, Icons } from '../../../uuiui'
import type { FontFamilyData } from './google-fonts-utils'

interface FontFamilyItem {
  style: React.CSSProperties
  data: FontFamilyData
  isOpen: boolean
  toggle: () => void
}

export const FontFamilyItem: React.FunctionComponent<React.PropsWithChildren<FontFamilyItem>> = ({
  style,
  data,
  isOpen,
  toggle,
}) => {
  return (
    <FlexRow style={style} onClick={toggle}>
      <div style={{ paddingLeft: 4, paddingRight: 4 }}>
        {isOpen ? <Icons.ExpansionArrowDown /> : <Icons.ExpansionArrowRight />}
      </div>
      <div style={{ flexGrow: 1, paddingBottom: 3 }}>{data.familyName}</div>
    </FlexRow>
  )
}
