/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { colorTheme } from '../../uuiui'
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

import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { printTree } from '../../core/shared/element-path-tree'
import { useDispatch } from '../editor/store/dispatch-context'
import CanvasActions from '../canvas/canvas-actions'
import { createInteractionViaPaste } from '../canvas/canvas-strategies/interaction-state'

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

  const dispatch = useDispatch()

  const jsxMetadata = useRefEditorState((store) => {
    return store.editor.jsxMetadata
  })

  const printEditorState = React.useCallback(() => {
    console.info('Current Editor State:', entireStateRef.current)
    console.info('Latest metadata:', jsxMetadata.current)
  }, [entireStateRef, jsxMetadata])

  const printElementPathTree = React.useCallback(() => {
    console.info('Tree:\n', printTree(entireStateRef.current.editor.elementPathTree))
  }, [entireStateRef])

  const startStaticReparentSession = React.useCallback(() => {
    dispatch([CanvasActions.createInteractionSession(createInteractionViaPaste())])
  }, [dispatch])

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
      }}
    >
      <span style={{ fontWeight: 800 }}>DEVELOPMENT</span>
      {perfTestTriggersEnabled ? (
        <React.Fragment>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={printEditorState}>PPP</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={startStaticReparentSession}>SSRS</a>
          </Tile>
          <Tile style={{ cursor: 'pointer', marginRight: 10 }} size='large'>
            <a onClick={printElementPathTree}>PT</a>
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
