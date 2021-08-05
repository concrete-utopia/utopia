import * as React from 'react'
import styled from '@emotion/styled'
//TODO: refactor components to functional components and use 'useColorTheme()':
import { colorTheme, UtopiaTheme } from './styles/theme'

export const InlineLink = styled.a({
  color: colorTheme.primary.value,
  textDecoration: 'none',
  cursor: 'pointer',
  paddingLeft: 2,
  paddingRight: 2,
  '&:hover': {
    textDecoration: 'underline',
  },
  '&:visited': {
    color: colorTheme.primary.value,
  },
  '&:focus': {
    textDecoration: 'underline',
  },
})

export const InlineButton = styled.button({
  fontSize: 11,
  fontFamily: 'utopian-inter',
  color: colorTheme.primary.value,
  background: 'transparent',
  border: 'none',
  outline: 'none',
  cursor: 'pointer',
  paddingLeft: 2,
  paddingRight: 2,
  borderRadius: UtopiaTheme.inputBorderRadius,
  position: 'relative',
  '&:hover': {
    borderRadius: 1,
    background: colorTheme.primary.shade(10).value,
  },
  '&:focus': {
    outline: 'none',
  },
  '&:active': {
    background: colorTheme.primary.value,
    color: colorTheme.neutralInvertedForeground.value,
    outline: 'none',
  },
})

export const InlineToggleButton = styled(InlineButton)<{
  value: boolean
}>((props) => ({
  background: props.value ? colorTheme.primary.value : 'transparent',
  color: props.value ? colorTheme.neutralInvertedForeground.value : colorTheme.primary.value,
  '&:hover': {
    background: props.value
      ? colorTheme.primary.shade(90).value
      : colorTheme.primary.shade(10).value,
    // color: props.value ? colorTheme.primary.shade(90).value : colorTheme.primary.shade(50).value,
  },
  '&:active': {
    background: props.value
      ? colorTheme.primary.shade(80).value
      : colorTheme.primary.shade(15).value,
  },
}))
