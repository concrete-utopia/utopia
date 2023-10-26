import React from 'react'
import { FlexRow, Icons, InspectorSubsectionHeader } from '../../../../../uuiui'
import { SeeMoreHOC, useToggle } from '../../../widgets/see-more'
import { PaddingRow } from '../../layout-section/layout-system-subsection/layout-system-controls'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { OverflowRow } from './overflow-row'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import { areElementsFlexContainersSelector } from '../../../flex-section'
import { unless } from '../../../../../utils/react-conditionals'

export const ContainerSubsection = React.memo(() => {
  const [seeMoreVisible, toggleSeeMoreVisible] = useToggle(false)

  const allElementsInFlexLayout = useEditorState(
    Substores.metadata,
    areElementsFlexContainersSelector,
    'ContainerSection areAllElementsInFlexLayout',
  )

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
          <span>Container</span>
        </FlexRow>
        <Icons.Threedots
          color={seeMoreVisible ? 'secondary' : 'subdued'}
          onClick={toggleSeeMoreVisible}
        />
      </InspectorSubsectionHeader>
      <OpacityRow />
      <OverflowRow />
      {unless(allElementsInFlexLayout, <PaddingRow />)}
      <SeeMoreHOC visible={seeMoreVisible}>
        <BlendModeRow />
      </SeeMoreHOC>
    </>
  )
})
