/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { colorTheme } from './styles/theme'

interface IconToggleButtonProps {
  value: boolean
  srcOn: string
  srcOff: string
  onToggle: (event: React.MouseEvent<HTMLButtonElement>) => void
  className?: string
}

export const IconToggleButton: React.FunctionComponent<IconToggleButtonProps> = (props) => {
  const { value, onToggle, srcOn, srcOff, className } = props
  return (
    <div
      role='button'
      tabIndex={0}
      className={className}
      css={{
        width: 22,
        height: 22,
        backgroundColor: value ? colorTheme.toggleButtonBackground.value : 'transparent',
        backgroundSize: 18,
        backgroundPosition: 'center',
        backgroundRepeat: 'no-repeat',
        outline: 'none',
        border: 'none',
        backgroundImage: value ? `url(${srcOn})` : `url(${srcOff})`,
        '&:focus-within': {
          border: `1px solid ${colorTheme.toggleButtonHoverBorder.value}`,
          outline: 'none',
        },
        '&:hover': {
          backgroundColor: colorTheme.toggleButtonHoverBackground.value,
          border: `1px solid ${colorTheme.toggleButtonHoverBorder.value}`,
        },
        '&:active': {
          transform: 'translateY(1px)',
          backgroundImage: value ? `url(${srcOff})` : `url(${srcOn})`,
        },
      }}
      onClick={onToggle}
    />
  )
}
