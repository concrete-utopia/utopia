import styled from '@emotion/styled'
import * as React from 'react'
import { UNSAFE_getIconURL } from './icn'
import { colorTheme } from './styles/theme'

export const InlineButton = styled.a({
  color: colorTheme.inlineButtonColor.value,
  textDecoration: 'none',
  cursor: 'pointer',
  paddingLeft: 2,
  paddingRight: 2,
  position: 'relative',
  '&:hover': {
    borderRadius: 1,
    background: '#E9F3FF',
  },
  '&:active': {
    background: colorTheme.inlineButtonColor.value,
    color: '#ffffff',
    borderRadiusTopLeft: 2,
    borderRadiusBottomLeft: 2,
  },

  '&:visited': {
    color: colorTheme.inlineButtonColor.value,
  },
})

export const InlinePopup = styled(InlineButton)({
  '&:hover::after': {
    content: '"_"',
    backgroundColor: '#E9F3FF',
    backgroundImage: UNSAFE_getIconURL('default', 'purple', 'component', 18, 18),
    backgroundPosition: 'center center',
    backgroundRepeat: 'no-repeat',
    backgroundSize: '9px 9px',
    position: 'absolute',
    width: 8,
    right: -8,
  },
  '&:active::after': {
    content: '"*"',
    backgroundColor: colorTheme.inlineButtonColor.value,
    backgroundImage: UNSAFE_getIconURL('expansionarrow-down', 'blue'),

    color: 'white',
    position: 'absolute',
    width: 10,
    right: -10,
    top: 0,
    bottom: 0,
  },
})
