import React from 'react'
import { Global, css } from '@emotion/react'
import { BaseCanvasOffsetLeftPane } from '../editor/store/editor-state'
import { useColorTheme } from '../../uuiui'

export const CanvasLoadingScreen = React.memo(() => {
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
            background: ${colorTheme.codeEditorShimmerPrimary.value};
            background: linear-gradient(
              to right,
              ${colorTheme.codeEditorShimmerPrimary.value} 8%,
              ${colorTheme.codeEditorShimmerSecondary.value} 18%,
              ${colorTheme.codeEditorShimmerPrimary.value} 33%
            );
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
            left: 0,
            top: 0,
            width: '100vw',
            height: '100vh',
          }}
        ></div>
      </div>
    </React.Fragment>
  )
})
