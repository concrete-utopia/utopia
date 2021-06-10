import styled from '@emotion/styled'
import { disabledOpacityStyle } from '../../styles/utopitrons'
import { colorTheme, UtopiaTheme } from '../../styles/theme'
import { FlexRow } from '../layout/flex-row'

export const Title = styled(FlexRow)({
  fontSize: 11,
  fontWeight: 600,
  letterSpacing: '0.1px',
})

export const H1 = styled(FlexRow)([
  disabledOpacityStyle,
  {
    fontSize: '10px',
    fontWeight: 600,
    textTransform: 'uppercase',
    letterSpacing: '0.2px',
  },
])

export const H2 = styled(FlexRow)([
  disabledOpacityStyle,
  {
    fontSize: '11px',
    fontWeight: 600,
    letterSpacing: '0.2px',
  },
])

export const H3 = styled(FlexRow)([
  disabledOpacityStyle,
  {
    fontSize: '10px',
    fontWeight: 500,
    letterSpacing: '0.1px',
    margin: '0px',
  },
])

export const Subdued = styled.span([
  disabledOpacityStyle,
  {
    color: colorTheme.subduedForeground.value,
    letterSpacing: '0.1px',
    lineHeight: '17px',
  },
])
export const VerySubdued = styled(Subdued)({
  color: colorTheme.verySubduedForeground.value,
})

export const InspectorSectionHeader = styled(H1)({
  label: 'section-header',
  height: 34,
  padding: `6px ${UtopiaTheme.layout.inspectorXPadding}px 6px ${UtopiaTheme.layout.inspectorXPadding}px`,
})
InspectorSectionHeader.displayName = 'InspectorSectionHeader'

export const InspectorSubsectionHeader = styled(H2)({
  height: UtopiaTheme.layout.rowHeight.normal,
  label: 'subsection-header',
  // no margin here so that subsections stack nicely
  // margin bottom needs to go into the subsection body
  padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px`,
})
InspectorSubsectionHeader.displayName = 'InspectorSubsectionHeader'
