import styled from '@emotion/styled'
import React from 'react'
import { Button } from '../../uuiui'

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
    padding: '1em 1.5em',
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
  const ButtonEl = styled(TitleButton)((props) => ({
    padding: '3px 12px 3px 8px',
    height: 36,
    borderRadius: 18,
    ...(color && { backgroundColor: color }),
  }))

  return (
    <ButtonEl highlight={true} onClick={onClick}>
      {children}
    </ButtonEl>
  )
}
