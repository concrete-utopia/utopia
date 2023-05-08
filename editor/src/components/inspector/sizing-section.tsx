import React from 'react'
import { FlexRow, InspectorSubsectionHeader } from '../../uuiui'
import { GroupDropdown } from './convert-to-group-dropdown'
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
          <GroupDropdown />
          <ResizeToFitControl />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexRow style={{ padding: 4, justifyContent: 'flex-end' }}>
        <FillHugFixedControl />
      </FlexRow>
    </>
  )
})
