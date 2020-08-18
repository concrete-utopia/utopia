import styled from '@emotion/styled'
import { disabledOpacityStyle } from '../../styles/utopitrons'
import { colorTheme, UtopiaTheme } from '../../styles/theme'
import { FlexRow } from '../layout/flex-row'

export const Title = styled(FlexRow)({
  fontSize: 11,
  fontWeight: 600,
  color: colorTheme.titleForeground.value,
  letterSpacing: '0.1px',
})

export const H1 = styled(FlexRow)([
  disabledOpacityStyle,
  {
    fontSize: '10px',
    fontWeight: 600,
    color: colorTheme.h1Foreground.value,
    textTransform: 'uppercase',
    letterSpacing: '0.2px',
  },
])

export const H2 = styled(FlexRow)([
  disabledOpacityStyle,
  {
    fontSize: '11px',
    color: colorTheme.h2Foreground.value,
    fontWeight: 600,
    letterSpacing: '0.2px',
  },
])

export const H3 = styled(FlexRow)([
  disabledOpacityStyle,
  {
    fontSize: '10px',
    fontWeight: 500,
    color: colorTheme.h3Foreground.value,
    letterSpacing: '0.1px',
    margin: '0px',
  },
])

export const Subdued = styled(FlexRow)([
  disabledOpacityStyle,
  {
    color: colorTheme.verySubduedForeground.value,
    letterSpacing: '0.1px',
  },
])

export const InspectorSectionHeader = styled(H1)({
  label: 'section-header',
  backgroundColor: colorTheme.sectionHeaderBackground.value,
  height: UtopiaTheme.layout.rowHeight.large,
  marginTop: 8,
  padding: `6px ${UtopiaTheme.layout.inspectorXPadding}px 6px ${UtopiaTheme.layout.inspectorXPadding}px`,
})
InspectorSectionHeader.displayName = 'InspectorSectionHeader'

export const InspectorSubsectionHeader = styled(H2)({
  height: UtopiaTheme.layout.rowHeight.medium,
  label: 'subsection-header',
  marginTop: 4,
  marginBottom: 4,
  // no margin here so that subsections stack nicely
  // margin bottom needs to go into the subsection body
  padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px`,
})
InspectorSubsectionHeader.displayName = 'InspectorSubsectionHeader'
