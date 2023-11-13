import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader } from '../../../../../uuiui'
import {
  EditorContractDropdown,
  allSelectedElementsContractSelector,
} from '../../../editor-contract-section'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { ClipContentControl } from './clip-content-control'
import { FrameUpdatingLayoutSection } from './frame-updating-layout-section'
import { RadiusRow } from '../../style-section/container-subsection/radius-row'
import { ResizeToFitControl } from '../../../resize-to-fit-control'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'

export const SimplifiedLayoutSubsection = React.memo(() => {
  const selectedElementContract = useEditorState(
    Substores.metadata,
    allSelectedElementsContractSelector,
    'EditorContractDropdown selectedElementContract',
  )

  const showLayoutSection =
    selectedElementContract === 'frame' || selectedElementContract === 'group'

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
          <ResizeToFitControl />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
        {showLayoutSection && (
          <>
            <FrameUpdatingLayoutSection />
            <UIGridRow
              padded={false}
              variant='<--1fr--><--1fr-->'
              style={{ minHeight: undefined, gap: 4 }}
            >
              <FixedHugDropdown dimension='width' />
              <FixedHugDropdown dimension='height' />
            </UIGridRow>
          </>
        )}
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
