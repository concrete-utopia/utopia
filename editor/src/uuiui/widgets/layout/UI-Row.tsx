import styled from '@emotion/styled'
import { commonSenseUtopiaLayoutShorthands } from './common-layout-shorthands'
import { flexRowStyle } from '../../styles/layout-styles'
import * as React from 'react'

export interface UIRowProp extends React.CSSProperties {
  padding?: number | string
  rowHeight?: number | string
}

export const UIRow = styled.div<UIRowProp>((props) => ({
  ...commonSenseUtopiaLayoutShorthands,
  ...flexRowStyle,
  padding: props.padding,
  height: props.height,
}))
UIRow.displayName = 'UIRow'
