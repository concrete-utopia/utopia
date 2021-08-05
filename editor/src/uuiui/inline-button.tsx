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

export const InlineIndicator = styled.div<{
  value: boolean
}>((props) => ({
  fontSize: 11,
  fontFamily: 'utopian-inter',
  background: 'transparent',
  border: 'none',
  outline: 'none',
  paddingLeft: 2,
  paddingRight: 2,
  color: props.value ? colorTheme.primary.value : colorTheme.primary.shade(30).value,
}))

export const InlineToggleButton = styled(InlineButton)<{
  value: boolean
}>((props) => ({
  color: props.value ? colorTheme.primary.value : colorTheme.primary.shade(30).value,
  '&:hover': {
    background: colorTheme.primary.shade(5).value,
    color: colorTheme.primary.shade(90).value,
  },
  '&:active': {
    background: colorTheme.primary.shade(10).value,
  },
}))
