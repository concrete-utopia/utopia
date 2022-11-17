/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import {
  useTriggerAbsoluteMoveLargePerformanceTest,
  useTriggerAbsoluteMoveSmallPerformanceTest,
  useTriggerAllElementsHighlightPerformanceTest,
  useTriggerRegularHighlightPerformanceTest,
  useTriggerResizePerformanceTest,
  useTriggerScrollPerformanceTest,
  useTriggerSelectionChangePerformanceTest,
  useTriggerSelectionPerformanceTest,
} from '../../core/model/performance-scripts'
import { useReParseOpenProjectFile } from '../../core/model/project-file-helper-hooks'
import { isFeatureEnabled } from '../../utils/feature-switches'

import { useEditorState, useRefEditorState } from '../editor/store/store-hook'

interface TileProps {
  size: 'smaller' | 'normal' | 'large' | 'max'
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
    console.info('Current Editor State:', entireStateRef.current)
    console.info('Latest metadata:', jsxMetadata.current)
  }, [entireStateRef, jsxMetadata])

  function useRequestVSCodeStatus(): () => void {
    const vscodeState = useEditorState(
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
  const onTriggerResizeTest = useTriggerResizePerformanceTest()
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
        borderRadius: '15px',
        border: '1px solid #95D4FF',
        paddingLeft: 8,
        paddingRight: 8,
        height: 25,
        width: 80,
        display: 'flex',
        alignItems: 'center',
        background: '#95D4FF22',
        overflow: 'scroll',
        color: '#95D4FF',
        fontSize: 8,
        fontWeight: 600,
        gap: 8,
      }}
    >
      <span style={{ fontWeight: 800 }}>Tests</span>
      {perfTestTriggersEnabled ? (
        <React.Fragment>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={printEditorState}>PPP</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerScrollTest}>P S</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={onTriggerResizeTest}>P R</a>
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
