import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader } from '../../../../../uuiui'
import { EditorContractDropdown } from '../../../editor-contract-section'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { ClipContentControl } from './clip-content-control'
import { FrameUpdatingLayoutSection } from './frame-updating-layout-section'
import { RadiusRow } from '../../style-section/container-subsection/radius-row'

export const SimplifiedLayoutSubsection = React.memo(() => {
  return (
    <FlexColumn style={{ paddingBottom: 8 }}>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            height: 42,
          }}
        >
          <EditorContractDropdown />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
        <FrameUpdatingLayoutSection />
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <FixedHugDropdown dimension='width' />
          <FixedHugDropdown dimension='height' />
        </UIGridRow>
        <FlexRow style={{ minHeight: undefined, gap: 4 }}>
          <RadiusRow />
        </FlexRow>
        <FlexRow style={{ minHeight: undefined, gap: 4 }}>
          <ClipContentControl />
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'
