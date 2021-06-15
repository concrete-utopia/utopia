import * as React from 'react'

export function Paragraph({ children, dark }) {
  return (
    <div
      className='md:text-xl sm:text-base md:leading-8 sm:leading-6 font-body sm:pb-4 md:pb-10'
      style={{
        color: dark ? '#D0D0D0' : '#383C4A',
      }}
    >
      {children}
    </div>
  )
}
