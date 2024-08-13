import React from 'react'
import { when } from '../../../../../utils/react-conditionals'
import { FlexColumn, FlexRow, InspectorSubsectionHeader, Tooltip } from '../../../../../uuiui'
import { Link } from '../../../../../uuiui/link'
import { useConvertWrapperToFrame } from '../../../../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import {
  EditorContractDropdown,
  allSelectedElementsContractSelector,
} from '../../../editor-contract-section'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import { ResizeToFitControl } from '../../../resize-to-fit-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { RadiusRow } from '../../style-section/container-subsection/radius-row'
import { ClipContentControl } from './clip-content-control'
import { FrameUpdatingLayoutSection } from './frame-updating-layout-section'

export const SimplifiedLayoutSubsection = React.memo(() => {
  const selectedElementContract = useEditorState(
    Substores.metadata,
    allSelectedElementsContractSelector,
    'EditorContractDropdown selectedElementContract',
  )

  const showLayoutSection =
    selectedElementContract === 'frame' || selectedElementContract === 'group'

  const showWrapperSectionWarning = selectedElementContract === 'wrapper-div'

  return (
    <FlexColumn>
      <InspectorSubsectionHeader style={{ border: 'none' }}>
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
      {when(showWrapperSectionWarning, <WrapperElementDisclosureBox />)}
      <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
        {when(
          showLayoutSection,
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
            <FlexRow style={{ minHeight: undefined, gap: 4 }}>
              <RadiusRow />
            </FlexRow>
            <FlexRow style={{ minHeight: undefined, gap: 4 }}>
              <ClipContentControl />
            </FlexRow>
          </>,
        )}
      </FlexColumn>
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'

const WrapperElementDisclosureBox = React.memo(() => {
  const convertToFrame = useConvertWrapperToFrame()
  return (
    <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
      <span
        style={{
          whiteSpace: 'initial',
          ['textWrap' as any]: 'balance', // this is an experimental Chrome feature
        }}
      >
        Element collapsed because it only has absolute children.
        <Tooltip title={'Convert element to Frame'} placement='left'>
          <Link onClick={convertToFrame}>Make it a Frame</Link>
        </Tooltip>
        to style and position it.
      </span>
    </FlexColumn>
  )
})
