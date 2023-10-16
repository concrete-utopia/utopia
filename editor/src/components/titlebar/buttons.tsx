/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { Button } from '../../uuiui'
import { colorTheme } from '../../uuiui'

interface TitleButtonProps {
  onMouseDown?: (e: React.MouseEvent<HTMLDivElement>) => void
  onClick: () => void
  color?: string
  testId?: string
}

const TitleButton = styled(Button)((props) => ({ fontWeight: 500, fontSize: 10 }))

export const SquareButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  color,
  children,
  testId,
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
      data-testid={testId}
    >
      {children}
    </ButtonEl>
  )
}

export const RoundButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onMouseDown,
  onClick,
  children,
}) => {
  const ButtonEl = styled(TitleButton)(() => ({
    padding: '3px',
    gap: 5,
    borderRadius: 20,
  }))

  return (
    <ButtonEl
      onClick={onClick}
      onMouseDown={onMouseDown}
      css={{
        height: 20,
        width: 20,
        '&:hover': {
          background: colorTheme.bg3.value,
        },
      }}
    >
      {children}
    </ButtonEl>
  )
}
