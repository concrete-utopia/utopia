import React from 'react'
import { useEditorState } from '../editor/store/store-hook'

export const UtopiaCanvasVarStyleTag = React.memo(() => {
  const roundedCanvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'DesignPanelRoot roundedCanvasOffset',
  )

  return (
    <style>{`
  .utopia-canvas-var-container {
    --utopia-canvas-offset-x: ${roundedCanvasOffset.x}px;
    --utopia-canvas-offset-y: ${roundedCanvasOffset.y}px;
  }
`}</style>
  )
})
