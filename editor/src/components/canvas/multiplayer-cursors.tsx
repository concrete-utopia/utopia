import React from 'react'
import type { MultiplayerCursorColor } from './multiplayer'

type MultiplayerCursorProps = {
  color: MultiplayerCursorColor | null
}

export const MultiplayerCursor = React.memo((props: MultiplayerCursorProps) => {
  return (
    <svg
      xmlns='http://www.w3.org/2000/svg'
      viewBox='0 0 35 35'
      fill='none'
      fillRule='evenodd'
      style={{
        position: 'absolute',
        top: -15,
        left: -15,
        width: 35,
        height: 35,
      }}
    >
      <g fill='rgba(0,0,0,.2)' transform='translate(1,1)'>
        <path d='m12 24.4219v-16.015l11.591 11.619h-6.781l-.411.124z' />
      </g>
      <g fill={'#fff'}>
        <path d='m12 24.4219v-16.015l11.591 11.619h-6.781l-.411.124z' />
      </g>
      <g fill={props.color?.background ?? '#000'}>
        <path d='m13 10.814v11.188l2.969-2.866.428-.139h4.768z' />
      </g>
    </svg>
  )
})

MultiplayerCursor.displayName = 'MultiplayerCursor'
