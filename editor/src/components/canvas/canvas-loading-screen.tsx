import React from 'react'
import { Global, css } from '@emotion/react'
import { betterReactMemo } from '../../uuiui-deps'
import { BaseCanvasOffsetLeftPane } from '../editor/store/editor-state'
import { useColorTheme } from '../../uuiui'

export const CanvasLoadingScreen = betterReactMemo('CanvasLoadingScreen', () => {
  const colorTheme = useColorTheme()
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
        style={{ height: '100%', width: '100%', backgroundColor: colorTheme.bg1.value }}
      >
        <div
          className='shimmer'
          style={{
            position: 'absolute',
            left: BaseCanvasOffsetLeftPane.x,
            top: BaseCanvasOffsetLeftPane.y,
            width: 375,
            height: 812,
          }}
        ></div>
      </div>
    </React.Fragment>
  )
})
