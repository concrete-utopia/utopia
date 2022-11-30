/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { colorTheme, FlexColumn } from '../../../../uuiui'

export const RefreshIcon: React.FC = () => {
  return (
    <FlexColumn>
      <svg
        xmlns='http://www.w3.org/2000/svg'
        width='12px'
        height='12px'
        viewBox='0 0 24 24'
        fill='none'
        stroke={colorTheme.fg0.value}
        strokeWidth='2'
        strokeLinecap='round'
        strokeLinejoin='round'
      >
        <polyline points='23 4 23 10 17 10'></polyline>
        <polyline points='1 20 1 14 7 14'></polyline>
        <path d='M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15'></path>
      </svg>
    </FlexColumn>
  )
}
