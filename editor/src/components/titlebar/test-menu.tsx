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
    console.info('Latest metadata:', jsxMetadata.current)
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
        paddingLeft: 8,
        paddingRight: 8,
        height: 25,
        width: 84,
        display: 'flex',
        alignItems: 'center',
        background: colorTheme.bg2.value,
        overflow: 'scroll',
        color: '#95D4FF',
        fontSize: 8,
        fontWeight: 600,
        gap: 8,
        pointerEvents: 'initial',
      }}
    >
      <span style={{ fontWeight: 800 }}>DEVELOPMENT</span>
      {perfTestTriggersEnabled ? (
        <React.Fragment>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={printEditorState}>PPP</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={printElementPathTree}>PT</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerScrollTest}>P S</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerRegularHighlightTest}>PRH</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerAllElementsHighlightTest}>PAH</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerSelectionTest}>P E</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerAbsoluteMoveLargeTest}>PAML</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerAbsoluteMoveSmallTest}>PAMS</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerSelectionChangeTest}>PSC</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onRequestVSCodeStatus}>VSC</a>
          </Tile>
        </React.Fragment>
      ) : null}
      {reParseProjectButtonEnabled ? (
        <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
          <a onClick={onReparseClick}>R</a>
        </Tile>
      ) : null}
    </div>
  )
})
