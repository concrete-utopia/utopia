/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { Button } from '../../uuiui'
import { colorTheme } from '../../uuiui'

interface TitleButtonProps {
  onClick: () => void
  color?: string
}

const TitleButton = styled(Button)((props) => ({ fontWeight: 500, fontSize: 10 }))

export const FullHeightButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  children,
}) => {
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '1em',
    height: '100%',
    borderRadius: 0,
  }))

  return (
    <ButtonEl primary={true} highlight={true} onClick={onClick}>
      {children}
    </ButtonEl>
  )
}

export const TextButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  children,
}) => {
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '1em 1.5em',
    height: '100%',
    borderRadius: 20,
    fontSize: 12,
  }))

  return (
    <ButtonEl highlight={true} onClick={onClick}>
      {children}
    </ButtonEl>
  )
}

export const RoundedButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  color,
  children,
}) => {
  const bgColor = color == null ? {} : { backgroundColor: color }
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '3px 12px 3px 8px',
    height: 36,
    borderRadius: 18,
    ...bgColor,
  }))

  return (
    <ButtonEl highlight={true} onClick={onClick}>
      {children}
    </ButtonEl>
  )
}

export const LozengeButton: React.FC<React.PropsWithChildren<TitleButtonProps>> = ({
  onClick,
  color,
  children,
}) => {
  const bgColor = color == null ? {} : { backgroundColor: color }
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '3px 12px',
    margin: '0px 3px',
    height: 22,
    borderRadius: 18,
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
