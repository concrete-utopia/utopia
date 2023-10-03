import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader } from '../../../../../uuiui'
import { EditorContractDropdown } from '../../../editor-contract-section'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { pinsLayoutNumberControl } from './gigantic-size-pins-subsection'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { BooleanControl } from '../../../controls/boolean-control'

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
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <FixedHugDropdown dimension='width' />
          <FixedHugDropdown dimension='height' />
        </UIGridRow>
        <FlexRow style={{ minHeight: undefined, gap: 4 }}>
          <ClipContentControl />
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'

const ClipContentControl = React.memo(() => {
  const isVisible = useIsSubSectionVisible('overflow')

  const { value, controlStatus, controlStyles, onSubmitValue, onUnsetValues } =
    useInspectorStyleInfo(
      'overflow',
      (parsed) => !parsed.overflow,
      (clipContent: boolean) => ({
        overflow: !clipContent,
      }),
    )

  if (!isVisible) {
    return null
  }

  return (
    <>
      <BooleanControl
        key='clip-content-control'
        id='clip-content-control'
        testId='clip-content-control'
        value={value}
        controlStatus={controlStatus}
        controlStyles={controlStyles}
        onSubmitValue={onSubmitValue}
      />
      Clip content
    </>
  )
})
