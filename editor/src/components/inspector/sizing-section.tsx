import React from 'react'
import { FlexRow, InspectorSubsectionHeader } from '../../uuiui'
import { FillHugFixedControlOld } from './fill-hug-fixed-control'
import { ResizeToFitFillFixedControl } from './resize-to-fit-control'

interface SizingSectionProps {}

export const SizingSection = React.memo<SizingSectionProps>(() => {
  return (
    <>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            height: 42,
          }}
        >
          <span style={{ flex: 1 }}>Resizing</span>
          <ResizeToFitFillFixedControl />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FillHugFixedControlOld />
    </>
  )
})
