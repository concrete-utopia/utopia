import React from 'react'
import styled from '@emotion/styled'
//TODO: refactor components to functional components and use 'useColorTheme()':
import { colorTheme, UtopiaTheme } from './styles/theme'

export const InlineLink = styled.a({
  color: colorTheme.primary.value,
  textDecoration: 'none',
  cursor: 'pointer',
  padding: '0 2px',
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
    background: colorTheme.canvasElementBackground.value,
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
  shouldIndicate: boolean
}>((props) => ({
  fontSize: 11,
  fontFamily: 'utopian-inter',
  background: 'transparent',
  border: 'none',
  outline: 'none',
  paddingLeft: 2,
  paddingRight: 2,
  color: props.shouldIndicate
    ? colorTheme.primary.value
    : colorTheme.canvasControlsInlineIndicatorInactive.value,
}))

export const InlineToggleButton = styled(InlineButton)<{
  toggleValue: boolean
}>((props) => ({
  color: props.toggleValue
    ? colorTheme.primary.value
    : colorTheme.canvasControlsInlineToggleUnsetText.value,
  '&:hover': {
    background: colorTheme.canvasControlsInlineToggleHoverBackground.value,
    color: colorTheme.canvasControlsInlineToggleHoverText.value,
  },
  '&:active': {
    background: colorTheme.canvasControlsInlineToggleActiveBackground.value,
  },
}))
