import React from 'react'
import * as EP from '../../../core/shared/element-path'
import { windowPoint } from '../../../core/shared/math-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { windowToCanvasCoordinates } from '../dom-lookup'

export const SelectionOutlineControl2 = React.memo(() => {
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'SelectionOutlineControl2',
  )
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'SelectionOutlineControl2 canvasOffset',
  )
  if (selectedElements.length === 1) {
    const target = selectedElements[0]
    const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(target)}"]`)
    const frame = htmlElement?.getBoundingClientRect()

    if (frame == null) {
      return null
    }
    const frameInCanvasCoords = windowToCanvasCoordinates(
      1,
      canvasOffset,
      windowPoint({ x: frame.x, y: frame.y }),
    )
    return (
      <div
        style={{
          position: 'absolute',
          left: frameInCanvasCoords.canvasPositionRounded.x + canvasOffset.x,
          top: frameInCanvasCoords.canvasPositionRounded.y + canvasOffset.y,
          width: frame.width,
          height: frame.height,
          backgroundColor: 'hotpink',
        }}
      ></div>
    )
  }
  return null
})
