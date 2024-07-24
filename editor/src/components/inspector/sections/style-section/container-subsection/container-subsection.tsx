import React from 'react'
import { colorTheme } from '../../../../../uuiui'
import { PaddingRow } from '../../layout-section/layout-system-subsection/layout-system-controls'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'

export const ContainerSubsection = React.memo(() => {
  return (
    <div style={{ borderTop: `1px solid ${colorTheme.seperator.value}`, padding: '10px 0' }}>
      <BlendModeRow />
      <OpacityRow />
      <PaddingRow />
    </div>
  )
})
