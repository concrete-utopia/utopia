import React from 'react'
import { colorTheme } from '../../../../../uuiui'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { PaddingRow } from './padding-row'
import { UIGridRow } from '../../../widgets/ui-grid-row'

export const ContainerSubsection = React.memo(() => {
  return (
    <div style={{ borderTop: `1px solid ${colorTheme.seperator.value}`, padding: '10px 0' }}>
      <UIGridRow padded variant={'<--1fr--><--1fr-->|22px|'}>
        <OpacityRow />
        <BlendModeRow />
      </UIGridRow>
      <PaddingRow />
    </div>
  )
})
