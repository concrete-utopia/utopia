/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { colorTheme, FlexColumn } from '../../../uuiui'

export const TrashIcon: React.FC = () => {
  return (
    <FlexColumn>
      <svg
        xmlns='http://www.w3.org/2000/svg'
        width='12'
        height='12'
        viewBox='0 0 24 24'
        fill={colorTheme.bg0.value}
        stroke={colorTheme.fg0.value}
        strokeWidth='2'
        strokeLinecap='round'
        strokeLinejoin='round'
      >
        <polyline points='3 6 5 6 21 6'></polyline>
        <path d='M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2'></path>
        <line x1='10' y1='11' x2='10' y2='17'></line>
        <line x1='14' y1='11' x2='14' y2='17'></line>
      </svg>
    </FlexColumn>
  )
}
