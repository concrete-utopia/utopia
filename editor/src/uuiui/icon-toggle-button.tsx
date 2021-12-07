/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { useColorTheme } from './styles/theme'

interface IconToggleButtonProps {
  value: boolean
  srcOn: string
  srcOff: string
  onToggle: (event: React.MouseEvent<HTMLDivElement>) => void
  className?: string
}

export const IconToggleButton = React.forwardRef<HTMLDivElement, IconToggleButtonProps>(
  (props, ref) => {
    const colorTheme = useColorTheme()
    const { value, onToggle, srcOn, srcOff, className } = props
    return (
      <div
        ref={ref}
        role='button'
        tabIndex={0}
        className={className}
        css={{
          width: 22,
          height: 22,
          cursor: 'pointer',
          backgroundColor: 'transparent',
          backgroundSize: 18,
          backgroundPosition: 'center',
          backgroundRepeat: 'no-repeat',
          outline: 'none',
          border: 'none',
          backgroundImage: value ? `url(${srcOn})` : `url(${srcOff})`,
          '&:focus-within': {
            outline: 'none',
          },
          '&:hover': {
            backgroundColor: colorTheme.buttonHoverBackground.value,
          },
          '&:active': {
            backgroundImage: value ? `url(${srcOff})` : `url(${srcOn})`,
            filter: 'brightness(99%)',
          },
        }}
        onClick={onToggle}
      />
    )
  },
)
