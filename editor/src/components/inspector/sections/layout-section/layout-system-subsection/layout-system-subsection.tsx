/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx, css } from '@emotion/react'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  LayoutSystemControl,
  DeleteAllLayoutSystemConfigButton,
  useLayoutSystemData,
} from './layout-system-controls'
import { FlexContainerControls } from '../flex-container-subsection/flex-container-subsection'
import { PropertyLabel } from '../../../widgets/property-label'
import {
  DetectedLayoutSystem,
  SpecialSizeMeasurements,
} from '../../../../../core/shared/element-template'
import {
  FlexRow,
  Icons,
  InspectorSectionHeader,
  InspectorSectionIcons,
  SquareButton,
  useColorTheme,
} from '../../../../../uuiui'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import { isNotUnsetOrDefault } from '../../../common/control-status'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'

interface LayoutSystemSubsectionProps {
  specialSizeMeasurements: SpecialSizeMeasurements
}

export const LayoutSystemSubsection = React.memo<LayoutSystemSubsectionProps>((props) => {
  const isFlexParent = props.specialSizeMeasurements.layoutSystemForChildren === 'flex'

  const hasAnyLayoutProps = isFlexParent
  const [layoutSectionOpen, setLayoutSectionOpen] = usePropControlledStateV2(hasAnyLayoutProps)

  const openSection = React.useCallback(() => setLayoutSectionOpen(true), [setLayoutSectionOpen])

  const layoutSystemData = useLayoutSystemData()

  const handleAddLayoutSystemClick = React.useCallback(() => {
    layoutSystemData.onSubmitValue('flex')
    openSection()
  }, [layoutSystemData, openSection])

  const colorTheme = useColorTheme()

  return (
    <React.Fragment>
      <InspectorSectionHeader
        css={{
          marginTop: 8,
          transition: 'color .1s ease-in-out',
          color: layoutSectionOpen ? colorTheme.fg1.value : colorTheme.fg7.value,
          '--buttonContentOpacity': 0.3,
          '&:hover': {
            color: colorTheme.fg1.value,
            '--buttonContentOpacity': 1,
          },
        }}
      >
        <FlexRow
          onClick={handleAddLayoutSystemClick}
          style={{
            flexGrow: 1,
            alignSelf: 'stretch',
            gap: 8,
          }}
        >
          <InspectorSectionIcons.LayoutSystem />
          <span>Layout System</span>
        </FlexRow>
        {layoutSectionOpen && <DeleteAllLayoutSystemConfigButton />}
        {!layoutSectionOpen && (
          <SquareButton highlight onClick={handleAddLayoutSystemClick}>
            <Icons.Plus style={{ opacity: 'var(--buttonContentOpacity)' }} />
          </SquareButton>
        )}
      </InspectorSectionHeader>
      {layoutSectionOpen ? (
        <React.Fragment>
          <UIGridRow padded={true} variant='<-------------1fr------------->'>
            <LayoutSystemControl
              layoutSystem={props.specialSizeMeasurements.layoutSystemForChildren}
              providesCoordinateSystemForChildren={
                props.specialSizeMeasurements.providesBoundsForAbsoluteChildren
              }
            />
          </UIGridRow>
          {isFlexParent ? <FlexContainerControls seeMoreVisible={true} /> : null}
        </React.Fragment>
      ) : null}
    </React.Fragment>
  )
})
