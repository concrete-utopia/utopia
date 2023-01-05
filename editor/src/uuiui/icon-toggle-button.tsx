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
        style={{
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
        }}
        onClick={onToggle}
      />
    )
  },
)
