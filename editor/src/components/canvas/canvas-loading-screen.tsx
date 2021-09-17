import * as React from 'react'
import { Global, css } from '@emotion/react'
import { betterReactMemo } from '../../uuiui-deps'

export const CanvasLoadingScreen = betterReactMemo('CanvasLoadingScreen', () => {
  return (
    <React.Fragment>
      <Global
        styles={css`
          @keyframes placeholderShimmer {
            0% {
              background-position: -468px 0;
            }
            100% {
              background-position: 468px 0;
            }
          }

          .shimmer {
            color: transparent;
            animation-name: placeholderShimmer;
            animation-duration: 1.25s;
            animation-fill-mode: forwards;
            animation-iteration-count: infinite;
            animation-timing-function: linear;
            background: #f6f6f6;
            background: linear-gradient(to right, #f6f6f6 8%, #f0f0f0 18%, #f6f6f6 33%);
            background-size: 800px 104px;
            position: relative;
          }
        `}
      />
      <div
        id='canvas-container-loading'
        className='shimmer'
        style={{ height: '100%', width: '100%' }}
      />
    </React.Fragment>
  )
})
