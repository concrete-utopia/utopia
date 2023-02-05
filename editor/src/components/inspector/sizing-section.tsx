import React from 'react'
import { FlexColumn, FlexRow, InspectorSectionIcons, InspectorSubsectionHeader } from '../../uuiui'
import { AdvancedControl } from './advanced-control'
import { FillHugFixedControl } from './fill-hug-fixed-control'
import { ResizeToFitControl } from './resize-to-fit-control'

interface SizingSectionProps {}

export const SizingSection = React.memo<SizingSectionProps>(() => {
  return (
    <>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
          }}
        >
          <InspectorSectionIcons.Layer />
          <span style={{ flexGrow: 1 }}>Size</span>
          <ResizeToFitControl />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexColumn style={{ padding: 4, gap: 16 }}>
        <FillHugFixedControl />
        <AdvancedControl />
      </FlexColumn>
    </>
  )
})
