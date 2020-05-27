import * as React from 'react'
import { betterReactMemo } from 'uuiui-deps'
import { GridRow } from '../../../widgets/grid-row'
import { InspectorSubsectionHeader } from 'uuiui'
import {
  LayoutSystemControl,
  FlexPaddingControl,
  paddingPropsToUnset,
  DeleteAllLayoutSystemConfigButton,
} from './layout-system-controls'
import { FlexContainerControls } from '../flex-container-subsection/flex-container-subsection'
import { PropertyLabel } from '../../../widgets/property-label'

interface LayoutSystemSubsectionProps {
  isFlexComponent: boolean
}

export const LayoutSystemSubsection = betterReactMemo<LayoutSystemSubsectionProps>(
  'LayoutSystemSubsection',
  (props) => {
    return (
      <>
        <InspectorSubsectionHeader>
          <span style={{ flexGrow: 1 }}>Layout System</span>
          <DeleteAllLayoutSystemConfigButton />
        </InspectorSubsectionHeader>
        <GridRow padded={true} type='<-------------1fr------------->'>
          <LayoutSystemControl />
        </GridRow>
        {props.isFlexComponent ? <FlexContainerControls seeMoreVisible={true} /> : null}
        <GridRow tall padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel
            target={paddingPropsToUnset}
            propNamesToUnset={['all paddings']}
            style={{ paddingBottom: 12 }}
          >
            Padding
          </PropertyLabel>
          <FlexPaddingControl />
        </GridRow>
      </>
    )
  },
)
