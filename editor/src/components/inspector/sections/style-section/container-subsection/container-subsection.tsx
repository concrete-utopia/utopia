import React from 'react'
import {
  FlexRow,
  H1,
  H2,
  Icons,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
} from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { PropertyLabel } from '../../../widgets/property-label'
import { SeeMoreButton, SeeMoreHOC, useToggle } from '../../../widgets/see-more'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  PaddingControl,
  paddingPropsToUnset,
} from '../../layout-section/layout-system-subsection/layout-system-controls'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { OverflowRow } from './overflow-row'
import { RadiusRow } from './radius-row'

export const ContainerSubsection = betterReactMemo('ContainerSubsection', () => {
  const [seeMoreVisible, toggleSeeMoreVisible] = useToggle(false)
  return (
    <>
      <InspectorSubsectionHeader style={{ borderTop: 'none' }}>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
          }}
        >
          <InspectorSectionIcons.Layer />
          <span>Container</span>
        </FlexRow>
        <Icons.Gear
          color={seeMoreVisible ? 'secondary' : 'subdued'}
          onClick={toggleSeeMoreVisible}
        />
      </InspectorSubsectionHeader>

      <OpacityRow />
      <OverflowRow />
      <RadiusRow />
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={paddingPropsToUnset}
          propNamesToUnset={['all paddings']}
          style={{ paddingBottom: 12 }}
        >
          Padding
        </PropertyLabel>
        <PaddingControl />
      </UIGridRow>
      <SeeMoreHOC visible={seeMoreVisible}>
        <BlendModeRow />
      </SeeMoreHOC>
    </>
  )
})
