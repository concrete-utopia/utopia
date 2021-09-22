import * as React from 'react'

export function Subtitle({ children, center, dark }) {
  return (
    <p
      className={
        'mt-14 sm:mt-16' +
        'sm:max-w-xl md:max-w-3xl ' +
        'text-xl md:text-2xl ' +
        'leading-snug md:leading-normal ' +
        'font-headline ' +
        'tracking-wider ' +
        'inline-block '
      }
      style={{
        color: dark ? '#D0D0D0' : '#383C4A',
      }}
    >
      {children}
    </p>
  )
}
