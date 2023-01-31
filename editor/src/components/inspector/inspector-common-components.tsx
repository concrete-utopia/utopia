import React from 'react'

interface DotProps {
  size: number
  bgColor: string
}

export const Dot = React.memo<DotProps>(({ size, bgColor }) => (
  <div
    style={{
      backgroundColor: bgColor,
      width: size,
      height: size,
      borderRadius: size / 2,
    }}
  />
))
