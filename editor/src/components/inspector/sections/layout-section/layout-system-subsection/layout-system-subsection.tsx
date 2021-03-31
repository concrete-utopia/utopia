import * as React from 'react'
import { UIGridRow } from '../../../widgets/uigrid-row'
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
              testId='layout-system-expand'
              visible
              collapsed={!layoutSectionOpen}
              selected={false}
            />
          </SquareButton>
        </InspectorSubsectionHeader>
        {layoutSectionOpen ? (
          <>
            <UIGridRow padded={true} type='<-------------1fr------------->'>
              <LayoutSystemControl
                layoutSystem={props.specialSizeMeasurements.layoutSystemForChildren}
                providesCoordinateSystemForChildren={
                  props.specialSizeMeasurements.providesBoundsForChildren
                }
              />
            </UIGridRow>
            {isFlexParent ? <FlexContainerControls seeMoreVisible={true} /> : null}
            <UIGridRow tall padded={true} type='<---1fr--->|------172px-------|'>
              <PropertyLabel
                target={paddingPropsToUnset}
                propNamesToUnset={['all paddings']}
                style={{ paddingBottom: 12 }}
              >
                Padding
              </PropertyLabel>
              <FlexPaddingControl />
            </UIGridRow>
          </>
        ) : null}
      </>
    )
  },
)
