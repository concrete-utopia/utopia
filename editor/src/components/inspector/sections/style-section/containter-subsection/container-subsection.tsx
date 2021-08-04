import * as React from 'react'
import { H2 } from '../../../../../uuiui'
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
      <UIGridRow padded={true} variant='<--------auto-------->||22px|'>
        <H2>Container</H2>
        <SeeMoreButton
          seeMoreVisible={seeMoreVisible}
          toggleSeeMoreVisible={toggleSeeMoreVisible}
        />
      </UIGridRow>
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
