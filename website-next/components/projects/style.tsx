/** @jsxRuntime classic */
/** @jsx jsx */
import styled from '@emotion/styled'

export const H2 = styled.h2({
  fontSize: 18,
  transform: 'rotate(-1deg)',
  fontWeight: 400,
  letterSpacing: '.2px',
})

export const InlineButton = styled.a({
  color: '#007AFF',
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
    background: '#007aff',
    color: '#ffffff',
    borderRadiusTopLeft: 2,
    borderRadiusBottomLeft: 2,
  },

  '&:visited': {
    color: '#007AFF',
  },
})

export const A = styled.a({
  color: '#007AFF',
  textDecoration: 'none',
  cursor: 'pointer',
  paddingLeft: 2,
  paddingRight: 2,
  '&:hover': {
    textDecoration: 'underline',
  },
  '&:visited': {
    color: '#007AFF',
  },
  '&:active': {},
})
