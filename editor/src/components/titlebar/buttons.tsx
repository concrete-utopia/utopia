/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React, { useState } from 'react'
import { Button, Icons } from '../../uuiui'
import { colorTheme } from '../../uuiui'
import { ThemeProvider } from '../../uuiui/styles/theme-provider'
import { PartialThemeObject } from '../../uuiui/styles/theme/types'

interface TitleButtonProps {
  onClick: () => void
  color?: string
  theme?: PartialThemeObject
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
    color: colorTheme.fg0.value,
    ...bgColor,
  }))

  return (
    <ThemeProvider theme={theme}>
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
    </ThemeProvider>
  )
}

export const PartialThemeButton: React.FC<React.PropsWithChildren<any>> = ({
  onClick,
  style,
  children,
  theme,
}) => {
  const [currentTheme, setCurrentTheme] = useState(theme)

  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '3px 12px',
    gap: 5,
    height: 24,
    borderRadius: 18,
    color: colorTheme.fg0.value,
    ...style,
  }))

  const toggleTheme = React.useCallback(() => {
    const newTheme = currentTheme == null ? theme : null
    setCurrentTheme(newTheme)
  }, [currentTheme, theme])

  return (
    <ThemeProvider theme={currentTheme}>
      <ButtonEl
        onClick={toggleTheme}
        css={{
          '&:hover': {
            ...style,
            opacity: 0.7,
          },
        }}
      >
        <PartialThemeButtonIcon />
        {children}
      </ButtonEl>
    </ThemeProvider>
  )
}

export const PartialThemeButtonIcon: React.FC = () => {
  return <Icons.WarningTriangle style={{ width: 19, height: 19 }} />
}
