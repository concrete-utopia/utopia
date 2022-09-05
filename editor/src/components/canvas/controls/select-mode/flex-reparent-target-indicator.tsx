import React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const FlexReparentTargetIndicator = React.memo(() => {
  const reparentTargetLines = useEditorState(
    (store) => store.editor.canvas.controls.flexReparentTargetLines,
    'FlexReparentTargetIndicator lines',
  )
  return (
    <CanvasOffsetWrapper>
      <div style={{ display: 'block' }}>
        {reparentTargetLines.map((line, i) => (
          <div
            key={i}
            style={{
              position: 'absolute',
              top: line.y,
              left: line.x,
              width: line.width,
              height: line.height,
              backgroundColor: 'blue',
            }}
          ></div>
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})
