import * as React from 'react'
import { GridRow } from '../../../widgets/grid-row'
import {
  LayoutSystemControl,
  FlexPaddingControl,
  paddingPropsToUnset,
  DeleteAllLayoutSystemConfigButton,
} from './layout-system-controls'
import { FlexContainerControls } from '../flex-container-subsection/flex-container-subsection'
import { PropertyLabel } from '../../../widgets/property-label'
import {
  DetectedLayoutSystem,
  SpecialSizeMeasurements,
} from '../../../../../core/shared/element-template'
import { InspectorSubsectionHeader, SquareButton } from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'
import { isNotUnsetOrDefault } from '../../../common/control-status'
import { ExpandableIndicator } from '../../../../navigator/navigator-item/expandable-indicator'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'

interface LayoutSystemSubsectionProps {
  specialSizeMeasurements: SpecialSizeMeasurements
}

export const LayoutSystemSubsection = betterReactMemo<LayoutSystemSubsectionProps>(
  'LayoutSystemSubsection',
  (props) => {
    const isFlexParent = props.specialSizeMeasurements.layoutSystemForChildren === 'flex'
    const paddings = useInspectorInfoLonghandShorthand(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      createLayoutPropertyPath,
    )
    const hasAnyLayoutProps =
      isFlexParent || Object.values(paddings).some((i) => isNotUnsetOrDefault(i.controlStatus))
    const [layoutSectionOpen, setLayoutSectionOpen] = usePropControlledStateV2(hasAnyLayoutProps)

    const toggleSection = React.useCallback(() => setLayoutSectionOpen(!layoutSectionOpen), [
      layoutSectionOpen,
      setLayoutSectionOpen,
    ])

    return (
      <>
        <InspectorSubsectionHeader>
          <span style={{ flexGrow: 1 }}>Layout System</span>
          {layoutSectionOpen && <DeleteAllLayoutSystemConfigButton />}
          <SquareButton highlight onClick={toggleSection}>
            <ExpandableIndicator
              testId='target-selector'
              visible
              collapsed={!layoutSectionOpen}
              selected={false}
            />
          </SquareButton>
        </InspectorSubsectionHeader>
        {layoutSectionOpen ? (
          <>
            <GridRow padded={true} type='<-------------1fr------------->'>
              <LayoutSystemControl
                layoutSystem={props.specialSizeMeasurements.layoutSystemForChildren}
                providesCoordinateSystemForChildren={
                  props.specialSizeMeasurements.providesBoundsForChildren
                }
              />
            </GridRow>
            {isFlexParent ? <FlexContainerControls seeMoreVisible={true} /> : null}
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
        ) : null}
      </>
    )
  },
)
