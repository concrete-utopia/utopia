import React from 'react'
import { FlexRow, InspectorSubsectionHeader } from '../../uuiui'
import { EditorContractDropdown } from './editor-contract-section'
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
            height: 42,
          }}
        >
          <div style={{ flex: 1 }} />
          <ResizeToFitControl />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexRow style={{ padding: 4, justifyContent: 'flex-end' }}>
        <FillHugFixedControl />
      </FlexRow>
    </>
  )
})
