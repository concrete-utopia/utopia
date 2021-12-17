import * as React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'
import { FlexAlignControlRectProps } from '../../canvas-types'

export const FlexAlignControls = React.memo(() => {
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FlexAlignControls canvasOffset',
  )
  const flexAlignDropTargets = useEditorState((store) => {
    return (
      (store.derived.canvas.transientState.editorStatePatch.canvas?.controls
        ?.flexAlignDropTargets as Array<FlexAlignControlRectProps>) ??
      store.editor.canvas.controls.flexAlignDropTargets
    )
  }, 'FlexAlignControls flexAlignDropTargets')

  return (
    <>
      {flexAlignDropTargets.map((rect) => {
        return (
          <div
            key={`rect-${rect.x}-${rect.y}-${rect.width}-${rect.height}`}
            style={{
              position: 'absolute',
              left: canvasOffset.x + rect.x,
              top: canvasOffset.y + rect.y,
              width: rect.width,
              height: rect.height,
              backgroundColor: rect.highlighted ? 'red' : 'blue',
            }}
          />
        )
      })}
    </>
  )
})
