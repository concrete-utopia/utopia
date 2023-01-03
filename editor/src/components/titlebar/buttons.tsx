/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { AltColorThemeProvider, Button } from '../../uuiui'
import { colorTheme } from '../../uuiui'
import { SubThemeObject } from '../../uuiui/styles/theme/types'

interface TitleButtonProps {
  onClick: () => void
  color?: string
  theme?: SubThemeObject
}

const TitleButton = styled(Button)((props) => ({ fontWeight: 500, fontSize: 10 }))

export const SquareButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  color,
  children,
}) => {
  const bgColor = color == null ? {} : { backgroundColor: color }
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '1em',
    height: 22,
    borderRadius: 3,
    color: colorTheme.fg9.value,
    ...bgColor,
  }))

  return (
    <ButtonEl
      onClick={onClick}
      css={{
        '&:hover': {
          background: color,
          opacity: 0.7,
        },
      }}
    >
      {children}
    </ButtonEl>
  )
}

export const RoundButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  color,
  children,
  theme,
}) => {
  const bgColor = color == null ? {} : { backgroundColor: color }
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '3px 12px',
    gap: 5,
    height: 24,
    borderRadius: 18,
    color: colorTheme.bg0.value,
    ...bgColor,
  }))

  return (
    <AltColorThemeProvider theme={theme}>
      <ButtonEl
        onClick={onClick}
        css={{
          '&:hover': {
            background: color,
            opacity: 0.7,
          },
        }}
      >
        {children}
      </ButtonEl>
    </AltColorThemeProvider>
  )
}
