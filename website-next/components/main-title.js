import * as React from 'react'

export function MainTitle({ children, dark }) {
  return (
    <h1
      className='md:text-6xl text-3xl leading-normal tracking-tight font-headline'
      style={{
        color: dark ? '#FFFFFF' : '#383C4A',
      }}
    >
      {children}
    </h1>
  )
}
