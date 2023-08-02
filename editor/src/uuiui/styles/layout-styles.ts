import type React from 'react'

export const flexRowStyle: Pick<
  React.CSSProperties,
  'display' | 'flexDirection' | 'alignItems' | 'whiteSpace'
> = {
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  whiteSpace: 'nowrap',
} as const

export const flexColumnStyle: Pick<
  React.CSSProperties,
  'display' | 'flexDirection' | 'whiteSpace'
> = {
  display: 'flex',
  flexDirection: 'column',
  whiteSpace: 'nowrap',
} as const

export const tileStyle: Pick<
  React.CSSProperties,
  'display' | 'flexDirection' | 'justifyContent' | 'alignItems'
> = {
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
} as const
