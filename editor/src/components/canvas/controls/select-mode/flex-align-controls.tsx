import * as React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'
import { useRoundedCanvasOffset } from '../../canvas-atoms'

export const FlexAlignControls = React.memo(() => {
  const [canvasOffset] = useRoundedCanvasOffset() // TODO do we need this, or can we use css-var?
  const flexAlignDropTargets = useEditorState((store) => {
    return store.editor.canvas.controls.flexAlignDropTargets
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
