import * as React from 'react'

export function MainTitle({ children, dark }) {
  return (
    <h1
      className='text-6xl leading-normal tracking-tight font-headline'
      style={{
        color: dark ? '#FFFFFF' : '#383C4A'
      }}
    >
      {children}
    </h1>
  )
}
