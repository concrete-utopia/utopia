import React from 'react'
import { FlexRow } from '../../uuiui'
import { CanvasToolbar } from '../editor/canvas-toolbar'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { ErrorOverlayComponent } from './canvas-error-overlay'
import { SafeModeErrorOverlay } from './canvas-wrapper-component'
import { CanvasStrategyPicker } from './controls/select-mode/canvas-strategy-picker'
import { TestMenu } from '../titlebar/test-menu'

export const CanvasFloatingToolbars = React.memo((props: { style: React.CSSProperties }) => {
  const safeMode = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.safeMode
    },
    'CanvasFloatingPanels safeMode',
  )

  return (
    <FlexRow
      style={{
        position: 'absolute',
        width: '100%',
        height: '100%',
        transform: 'translateZ(0)', // to keep this from tarnishing canvas render performance, we force it to a new layer
        pointerEvents: 'none', // you need to re-enable pointerevents for the various overlays
        ...props.style,
      }}
    >
      <FlexRow
        style={{
          position: 'absolute',
          top: 0,
          alignItems: 'flex-start',
          margin: 10,
          gap: 10,
        }}
      >
        <CanvasToolbar />
        <CanvasStrategyPicker />
        <TestMenu />
      </FlexRow>
      {/* The error overlays are deliberately the last here so they hide other canvas UI */}
      {safeMode ? <SafeModeErrorOverlay /> : <ErrorOverlayComponent />}
    </FlexRow>
  )
})
CanvasFloatingToolbars.displayName = 'CanvasFloatingToolbars'
