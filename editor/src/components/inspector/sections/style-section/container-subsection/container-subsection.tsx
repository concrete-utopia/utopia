import React from 'react'
import { FlexRow, InspectorSubsectionHeader, colorTheme } from '../../../../../uuiui'
import { PaddingRow } from '../../layout-section/layout-system-subsection/layout-system-controls'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { UIGridRow } from '../../../widgets/ui-grid-row'

export const ContainerSubsection = React.memo(() => {
  return (
    <div style={{ borderTop: `1px solid ${colorTheme.seperator.value}`, padding: '10px 0' }}>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            height: 42,
          }}
        >
          <span>Container</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      <UIGridRow padded variant={'<--1fr--><--1fr-->|22px|'}>
        <OpacityRow />
        <BlendModeRow />
      </UIGridRow>
      <PaddingRow />
    </div>
  )
})
