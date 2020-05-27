import styled from '@emotion/styled'
import { css } from '@emotion/core'

export const flexRowStyle: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  whiteSpace: 'nowrap',
}

export const flexColumnStyle: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  whiteSpace: 'nowrap',
}

interface CommonSenseUtopiaProps {
  flexGrow?: number
}

export const commonSenseUtopiaLayoutShorthands = (props: CommonSenseUtopiaProps) =>
  css({
    flexGrow: props.flexGrow,
  })

export const FlexRow = styled.div([commonSenseUtopiaLayoutShorthands, { ...flexRowStyle }])
FlexRow.displayName = 'FlexRow'

export const FlexColumn = styled.div([commonSenseUtopiaLayoutShorthands, { ...flexColumnStyle }])
FlexRow.displayName = 'FlexColumn'
