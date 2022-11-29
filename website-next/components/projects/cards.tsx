import * as React from 'react'
import styled from '@emotion/styled'

import { layout } from './layout'
import { colors, darkColors } from './theme'

export const cardLayout = {
  width: 310,
  height: 220,
  footerHeight: 60,
  imageHeight: 160,
}

export const cardLayoutStyle = {
  flexBasis: cardLayout.width,
  flexShrink: 1,
  height: cardLayout.height,
  minWidth: cardLayout.width,
}

interface CardProps {
  selected: boolean
}

export const Card = styled('div')<CardProps>({ ...cardLayoutStyle }, (props) => ({
  boxShadow: props.selected ? `0px 0px 0px 2px  ${colors.primary}` : '0px 0px 0px 1px  black',
  borderRadius: '5px',
  overflow: 'hidden',
  ['@media (prefers-color-scheme: dark)']: {
    backgroundColor: darkColors.background,
    boxShadow: props.selected
      ? `0px 0px 0px 2px  ${colors.primary}`
      : `0px 0px 0px 1px ${darkColors.default}`,
  },
}))
