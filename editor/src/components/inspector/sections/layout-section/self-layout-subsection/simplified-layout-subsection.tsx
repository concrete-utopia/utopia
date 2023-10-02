import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader } from '../../../../../uuiui'
import { EditorContractDropdown } from '../../../editor-contract-section'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { pinsLayoutNumberControl } from './gigantic-size-pins-subsection'

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
      <FlexColumn style={{ gap: 8 }}>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          {pinsLayoutNumberControl('left')}
          {pinsLayoutNumberControl('top')}
        </UIGridRow>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          {pinsLayoutNumberControl('width')}
          {pinsLayoutNumberControl('height')}
        </UIGridRow>
      </FlexColumn>
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'
