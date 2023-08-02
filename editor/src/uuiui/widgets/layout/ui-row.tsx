import styled from '@emotion/styled'
import { commonSenseUtopiaLayoutShorthands } from './common-layout-shorthands'
import { flexRowStyle } from '../../styles/layout-styles'
import type React from 'react'
import { UtopiaTheme } from '../../styles/theme'

export interface UIRowProp extends React.InputHTMLAttributes<HTMLDivElement> {
  padded?: boolean
  rowHeight?: keyof typeof UtopiaTheme.layout.rowHeight
}

export const UIRow = styled.div<UIRowProp>((props) => ({
  ...commonSenseUtopiaLayoutShorthands,
  ...flexRowStyle,
  padding: props.padded === true ? UtopiaTheme.layout.rowHorizontalPadding : undefined,
  height:
    props.rowHeight == null
      ? UtopiaTheme.layout.rowHeight.normal
      : UtopiaTheme.layout.rowHeight[props.rowHeight],
}))
UIRow.displayName = 'UIRow'
