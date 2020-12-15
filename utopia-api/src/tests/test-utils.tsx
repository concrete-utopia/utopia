import styled from '@emotion/styled'
import { css } from '@emotion/react'

export const flexRowStyle: Pick<
  React.CSSProperties,
  'display' | 'flexDirection' | 'alignItems' | 'whiteSpace'
> = {
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  whiteSpace: 'nowrap',
}

export const flexColumnStyle: Pick<
  React.CSSProperties,
  'display' | 'flexDirection' | 'alignItems' | 'whiteSpace'
> = {
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
