/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { UtopiaStyles, colorTheme } from '../../uuiui'
import {
  useTriggerAbsoluteMoveLargePerformanceTest,
  useTriggerAbsoluteMoveSmallPerformanceTest,
  useTriggerAllElementsHighlightPerformanceTest,
  useTriggerRegularHighlightPerformanceTest,
  useTriggerScrollPerformanceTest,
  useTriggerSelectionChangePerformanceTest,
  useTriggerSelectionPerformanceTest,
} from '../../core/model/performance-scripts'
import { useReParseOpenProjectFile } from '../../core/model/project-file-helper-hooks'
import { isFeatureEnabled } from '../../utils/feature-switches'

import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { printTree } from '../../core/shared/element-path-tree'
import { useDispatch } from '../editor/store/dispatch-context'
import type { EditorStorePatched } from '../editor/store/editor-state'
import { objectMap } from '../../core/shared/object-utils'
import { fromString, humanReadableDebugPath } from '../../core/shared/element-path'

interface TileProps {
  size: 'smaller' | 'normal' | 'large' | 'max'
}

declare global {
  interface Window {
    entireState: EditorStorePatched
  }
}

const Tile = styled.div<TileProps>((props) => ({
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
  width: props.size,
}))

export const TestMenu = React.memo(() => {
  const entireStateRef = useRefEditorState((store) => store)

  const jsxMetadata = useRefEditorState((store) => {
    return store.editor.jsxMetadata
  })

  const printEditorState = React.useCallback(() => {
    window.entireState = entireStateRef.current
    console.info('Current Editor State: run `window.entireState` in console')
    console.info(
      'Latest metadata:',
      Object.fromEntries(
        Object.entries(jsxMetadata.current).map(([key, value]) => [
          humanReadableDebugPath(fromString(key)),
          value,
        ]),
      ),
    )
  }, [entireStateRef, jsxMetadata])

  const printElementPathTree = React.useCallback(() => {
    console.info('Tree:\n', printTree(entireStateRef.current.editor.elementPathTree))
  }, [entireStateRef])

  function useRequestVSCodeStatus(): () => void {
    const vscodeState = useEditorState(
      Substores.restOfEditor,
      (store) => ({
        vscodeReady: store.editor.vscodeReady,
        loadingScreenVisible: store.editor.vscodeLoadingScreenVisible,
      }),
      'useRequestVSCodeStatus',
    )

    return React.useCallback(
      // eslint-disable-next-line no-console
      () => console.log(`VSCode State: ${JSON.stringify(vscodeState)}`),
      [vscodeState],
    )
  }

  const onReparseClick = useReParseOpenProjectFile()

  const onTriggerScrollTest = useTriggerScrollPerformanceTest()
  const onTriggerRegularHighlightTest = useTriggerRegularHighlightPerformanceTest()
  const onTriggerAllElementsHighlightTest = useTriggerAllElementsHighlightPerformanceTest()
  const onTriggerSelectionTest = useTriggerSelectionPerformanceTest()
  const onTriggerAbsoluteMoveLargeTest = useTriggerAbsoluteMoveLargePerformanceTest()
  const onTriggerAbsoluteMoveSmallTest = useTriggerAbsoluteMoveSmallPerformanceTest()
  const onTriggerSelectionChangeTest = useTriggerSelectionChangePerformanceTest()

  const onRequestVSCodeStatus = useRequestVSCodeStatus()

  const perfTestTriggersEnabled = isFeatureEnabled('Performance Test Triggers'),
    reParseProjectButtonEnabled = isFeatureEnabled('Re-parse Project Button')

  if (!perfTestTriggersEnabled && !reParseProjectButtonEnabled) return null

  return (
    <div
      style={{
        position: 'absolute',
        bottom: 6,
        boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
        borderRadius: '15px',

        alignItems: 'center',
        justifyContent: 'center',
        background: colorTheme.bg2.value,
        overflow: 'hidden',
        color: '#95D4FF',
        fontSize: 6,
        fontWeight: 600,
        pointerEvents: 'initial',
        padding: 8,
      }}
      css={{
        '&:hover > div': {
          height: 120,
          overflow: 'scroll',
        },
      }}
    >
      <div
        style={{
          width: 48,
        }}
        css={{
          height: 14,
          overflow: 'visible',
          transition: 'all 0.1s ease-in-out',
        }}
      >
        <div style={{ fontWeight: 800, textAlign: 'center', width: '100%' }}>DEVELOPMENT</div>
        <div
          style={{
            display: 'grid',
            gridTemplateColumns: 'repeat(2, 1fr)',
            alignItems: 'center',
            justifyContent: 'center',
            gap: 8,
            width: '100%',
          }}
        >
          {perfTestTriggersEnabled ? (
            <React.Fragment>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={printEditorState} title='Print Editor State'>
                  PPP
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={printElementPathTree} title='Print Element Path Tree'>
                  PT
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onTriggerScrollTest} title='Performance Scroll'>
                  P S
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onTriggerRegularHighlightTest} title='Performance Regular Highlight'>
                  PRH
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a
                  onClick={onTriggerAllElementsHighlightTest}
                  title='Performance All Elements Highlight'
                >
                  PAH
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onTriggerSelectionTest} title='Performance Elements'>
                  P E
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onTriggerAbsoluteMoveLargeTest} title='Performance Absolute Move Large'>
                  PAML
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onTriggerAbsoluteMoveSmallTest} title='Performance Absolute Move Small'>
                  PAMS
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onTriggerSelectionChangeTest} title='Performance Selection Change'>
                  PSC
                </a>
              </Tile>
              <Tile style={{ cursor: 'pointer' }} size='large'>
                <a onClick={onRequestVSCodeStatus} title='VS Code Status'>
                  VSC
                </a>
              </Tile>
            </React.Fragment>
          ) : null}
          {reParseProjectButtonEnabled ? (
            <Tile style={{ cursor: 'pointer' }} size='large'>
              <a onClick={onReparseClick} title='Reparse Project'>
                R
              </a>
            </Tile>
          ) : null}
        </div>
      </div>
    </div>
  )
})
