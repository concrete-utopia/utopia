import * as React from 'react'
import styled from '@emotion/styled'

import { layout } from './layout'
import { colors } from './theme'

export const cardLayout = {
  width: 340,
  height: 220,
  footerHeight: 60,
  imageHeight: 160,
}

export const cardLayoutStyle = {
  flexBasis: cardLayout.width,
  flexGrow: 1,
  flexShrink: 1,
  height: cardLayout.height,
  maxWidth: cardLayout.width + layout.margins.wide,
  minWidth: cardLayout.width - 2 * layout.margins.wide,
  margin: layout.margins.regular,
}

interface CardProps {
  selected: boolean
}

export const Card = styled('div')<CardProps>({ ...cardLayoutStyle }, (props) => ({
  boxShadow: props.selected ? `0px 0px 0px 2px  ${colors.primary}` : '0px 0px 0px 1px  black',
}))
