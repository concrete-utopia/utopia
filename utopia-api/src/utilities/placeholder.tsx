import React from 'react'

export interface PlaceholderProps {
  style?: React.CSSProperties
  margin?: boolean
  fill?: boolean
  fixed?: boolean
}

export const Placeholder = ({ style, margin, fill, fixed = true }: PlaceholderProps) => (
  <div
    style={{
      backgroundColor: 'rgb(225 225 225 / 25%)',
      border: '1px dashed rgb(61 154 255)',
      borderRadius: 10,
      minWidth: fixed ? 100 : undefined,
      minHeight: fixed ? 100 : undefined,
      flexGrow: fill ? 1 : undefined,
      alignSelf: fill ? 'stretch' : undefined,
      margin: margin ? '10px' : undefined,
      ...style,
    }}
  />
)
