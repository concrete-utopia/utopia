import React from 'react'
import { useContextSelector } from 'use-context-selector'
import {
  FlexRow,
  H1,
  H2,
  Icons,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
} from '../../../../../uuiui'
import { InspectorPropsContext } from '../../../common/property-path-hooks'
import { PropertyLabel } from '../../../widgets/property-label'
import { SeeMoreButton, SeeMoreHOC, useToggle } from '../../../widgets/see-more'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  PaddingControl,
  buildPaddingPropsToUnset,
} from '../../layout-section/layout-system-subsection/layout-system-controls'
import { BlendModeRow } from './blendmode-row'
import { OpacityRow } from './opacity-row'
import { OverflowRow } from './overflow-row'
import { RadiusRow } from './radius-row'

export const ContainerSubsection = React.memo(() => {
  const [seeMoreVisible, toggleSeeMoreVisible] = useToggle(false)
  const targetPath = useContextSelector(
    InspectorPropsContext,
    React.useCallback((contextData) => {
      return contextData.targetPath
    }, []),
  )
  const paddingPropsToUnset = React.useMemo(() => {
    return buildPaddingPropsToUnset(targetPath)
  }, [targetPath])
  return (
    <>
      <InspectorSubsectionHeader>
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
