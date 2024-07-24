import React from 'react'
import { colorTheme } from '../../../../../uuiui'
import { PaddingRow } from '../../layout-section/layout-system-subsection/layout-system-controls'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { UIGridRow } from '../../../widgets/ui-grid-row'

export const ContainerSubsection = React.memo(() => {
  return (
    <div style={{ borderTop: `1px solid ${colorTheme.seperator.value}`, padding: '10px 0' }}>
      <UIGridRow padded variant={'<--1fr--><--1fr-->'}>
        <BlendModeRow />
        <OpacityRow />
      </UIGridRow>
      <PaddingRow />
    </div>
  )
})
