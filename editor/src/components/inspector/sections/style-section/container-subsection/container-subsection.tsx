import React from 'react'
import { FlexRow, Icons, InspectorSubsectionHeader } from '../../../../../uuiui'
import { SeeMoreHOC, useToggle } from '../../../widgets/see-more'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { OverflowRow } from './overflow-row'
import { PaddingRow } from './padding-row'

export const ContainerSubsection = React.memo(() => {
  const [seeMoreVisible, toggleSeeMoreVisible] = useToggle(false)

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
      <PaddingRow />
      <SeeMoreHOC visible={seeMoreVisible}>
        <BlendModeRow />
      </SeeMoreHOC>
    </>
  )
})
