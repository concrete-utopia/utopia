import * as React from 'react'

export const flexRowStyle: Pick<
  React.CSSProperties,
  'display' | 'flexDirection' | 'alignItems' | 'whiteSpace'
> = {
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  whiteSpace: 'nowrap',
}

export const flexColumnStyle: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  whiteSpace: 'nowrap',
}

export const tileStyle: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
}
