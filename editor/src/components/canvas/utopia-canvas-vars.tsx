import React from 'react'
import { CanvasScale, CanvasScrollOffset } from '../../utils/global-positions'
import { useEditorState } from '../editor/store/store-hook'

export const UtopiaCanvasVars = React.memo(() => {
  const roundedCanvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'DesignPanelRoot roundedCanvasOffset',
  )
  const canvasScale = useEditorState(
    (store) => store.editor.canvas.scale,
    'DesignPanelRoot roundedCanvasOffset',
  )

  CanvasScrollOffset.x = roundedCanvasOffset.x
  CanvasScrollOffset.y = roundedCanvasOffset.y
  CanvasScale.current = canvasScale
  return null
})
