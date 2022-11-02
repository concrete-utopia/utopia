/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import React from 'react'
import { FlexColumn } from '../../../../uuiui'

export const GithubSpinner: React.FC = () => {
  const anim = keyframes`
        from {
          transform: rotate(0deg);
        }
        to {
          transform: rotate(360deg);
        }
      `

  return (
    <FlexColumn>
      <svg
        xmlns='http://www.w3.org/2000/svg'
        width='16'
        height='16'
        viewBox='0 0 24 24'
        fill='none'
        stroke='#999'
        strokeWidth='2'
        strokeLinecap='round'
        strokeLinejoin='round'
        css={css`
          animation: ${anim} 1s linear infinite;
        `}
      >
        <line x1='12' y1='2' x2='12' y2='6'></line>
        <line x1='12' y1='18' x2='12' y2='22'></line>
        <line x1='4.93' y1='4.93' x2='7.76' y2='7.76'></line>
        <line x1='16.24' y1='16.24' x2='19.07' y2='19.07'></line>
        <line x1='2' y1='12' x2='6' y2='12'></line>
        <line x1='18' y1='12' x2='22' y2='12'></line>
        <line x1='4.93' y1='19.07' x2='7.76' y2='16.24'></line>
        <line x1='16.24' y1='7.76' x2='19.07' y2='4.93'></line>
      </svg>
    </FlexColumn>
  )
}
