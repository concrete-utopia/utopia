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
import { DetectedLayoutSystem } from '../../../../../core/shared/element-template'

interface LayoutSystemSubsectionProps {
  layoutSystem: DetectedLayoutSystem | null
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
          <LayoutSystemControl layoutSystem={props.layoutSystem} />
        </GridRow>
        {props.layoutSystem === 'flex' ? <FlexContainerControls seeMoreVisible={true} /> : null}
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel target={paddingPropsToUnset} propNamesToUnset={['all paddings']}>
            Padding
          </PropertyLabel>
          <FlexPaddingControl />
        </GridRow>
      </>
    )
  },
)
