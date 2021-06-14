import * as React from 'react'

export function Paragraph({ children, dark }) {
  return (
    <div
      className='text-2xl font-body pb-10'
      style={{
        textShadow: '0 2px 59px #00FFCD',
        color: dark ? '#D0D0D0' : '#383C4A'
      }}
    >
      {children}
    </div>
  )
}