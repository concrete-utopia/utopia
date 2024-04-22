import React from 'react'

interface PlaceholderProps {
  style?: React.CSSProperties
}

export const Placeholder = React.memo((props: PlaceholderProps) => {
  return (
    <div
      {...props}
      style={{
        ...(props.style ?? {}),
        border: '1px solid #e7e7e7',
        borderRadius: '0 10% 0 10%',
      }}
    />
  )
})
