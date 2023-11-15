import React from 'react'
import {
  Button,
  FlexColumn,
  FlexRow,
  InspectorSubsectionHeader,
  Tooltip,
  colorTheme,
} from '../../../../../uuiui'
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
import { NO_OP } from '../../../../../core/shared/utils'
import { when } from '../../../../../utils/react-conditionals'
import { useConvertWrapperToFrame } from '../../../../canvas/canvas-strategies/strategies/group-conversion-helpers'

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
        The selected element appears to be a collapsed wrapper element around absolutely positioned
        children. If you want to explicitly set its size and layout, convert it to a fixed sized
        element!
      </span>
      <FlexRow
        style={{
          flexGrow: 1,
          gap: 8,
          height: 42,
        }}
      >
        <Tooltip title={'Convert element to Frame'} placement='left'>
          <Button
            highlight
            spotlight
            data-testid={'convert-to-frame-button'}
            style={{
              backgroundColor: colorTheme.errorForeground20.value,
              color: colorTheme.errorForeground.value,
              padding: '0 6px',
              borderRadius: 2,
            }}
            onClick={convertToFrame}
          >
            Convert to Frame
          </Button>
        </Tooltip>
      </FlexRow>
    </FlexColumn>
  )
})
