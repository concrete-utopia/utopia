import React from 'react'
import { FlexRow, UtopiaStyles, colorTheme } from '../../../uuiui'

export const RootElementIndicator = () => {
  return (
    <FlexRow
      style={{
        justifyContent: 'center',
        alignItems: 'center',
        background: colorTheme.inspectorBackground.value,
      }}
    >
      <div
        key={'root-element-indicator'}
        style={{
          ...UtopiaStyles.noticeStyles.error,
          borderRadius: 6,
          padding: 5,
          margin: 7,
          flex: 1,
        }}
      >
        <span>A Root Element Is Selected!</span>
      </div>
    </FlexRow>
  )
}
