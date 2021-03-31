import styled from '@emotion/styled'
import { commonSenseUtopiaLayoutShorthands } from './common-layout-shorthands'
import { flexRowStyle } from '../../styles/layout-styles'
import * as React from 'react'
import { UtopiaTheme } from '../../styles/theme'

export interface UIRowProp extends React.InputHTMLAttributes<HTMLDivElement> {
  padding?: boolean
  rowHeight?: keyof typeof UtopiaTheme.layout.rowHeight
}

export const UIRow = styled.div<UIRowProp>((props) => ({
  ...commonSenseUtopiaLayoutShorthands,
  ...flexRowStyle,
  padding: props.padding ? UtopiaTheme.layout.rowHorizontalPadding : undefined,
  height: props.rowHeight ?? UtopiaTheme.layout.rowHeight.normal,
}))
UIRow.displayName = 'UIRow'
