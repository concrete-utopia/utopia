import styled from '@emotion/styled'
import React from 'react'
//TODO: refactor components to functional components and use 'useColorTheme()':
import { colorTheme } from './styles/theme'

export const Link = styled.a({
  color: colorTheme.inlineButtonColor.value,
  textDecoration: 'none',
  cursor: 'pointer',
  paddingLeft: 2,
  paddingRight: 2,
  '&:hover': {
    textDecoration: 'underline',
  },
  '&:visited': {
    color: colorTheme.inlineButtonColor.value,
  },
})
