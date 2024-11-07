import React from 'react'
import { Global, css } from '@emotion/react'
import { useColorTheme } from '../../uuiui'
import { useEditorState } from '../editor/store/store-hook'
import { Substores } from '../editor/store/store-hook'

export const CanvasLoadingScreen = React.memo(() => {
  const colorTheme = useColorTheme()
  const importState = useEditorState(
    Substores.github,
    (store) => store.editor.importState,
    'CanvasLoadingScreen importState',
  )
  const importWizardOpen = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.importWizardOpen,
    'CanvasLoadingScreen importWizardOpen',
  )

  const importingStoppedStyleOverride = React.useMemo(
    () =>
      // if the importing was stopped, we want to pause the shimmer animation
      (importWizardOpen && importState.importStatus.status === 'done') ||
      importState.importStatus.status === 'paused'
        ? {
            background: colorTheme.codeEditorShimmerPrimary.value,
            animation: 'none',
          }
        : {},
    [importWizardOpen, importState.importStatus.status, colorTheme.codeEditorShimmerPrimary.value],
  )

  return (
    <React.Fragment>
      <Global
        styles={css`
          @keyframes placeholderShimmer {
            0% {
              background-position: -1468px 0;
            }
            100% {
              background-position: 1468px 0;
            }
          }

          .shimmer {
            color: transparent;
            animation-name: placeholderShimmer;
            animation-duration: 4.25s;
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
            background-size: 1468px 104px;
            position: relative;
          }

          .no-shimmer {
            animation: none;
            background: ${colorTheme.codeEditorShimmerPrimary.value};
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
            ...importingStoppedStyleOverride,
          }}
        ></div>
      </div>
    </React.Fragment>
  )
})
