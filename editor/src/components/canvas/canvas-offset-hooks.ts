import React from 'react'
import { CanvasVector } from '../../core/shared/math-utils'
import { useRefEditorState, useSelectorWithCallback } from '../editor/store/store-hook'

export function useCanvasOffset(elementRef: React.RefObject<HTMLDivElement>): void {
  const offsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const transformCanvasOffset = React.useCallback(
    (canvasOffset: CanvasVector) => {
      // console.log('TRANFORM CANVASOFFSET', elementRef.current, canvasOffset.x, canvasOffset.y)
      if (elementRef.current != null) {
        elementRef.current.style.setProperty(
          'transform',
          `translate(${canvasOffset.x}px, ${canvasOffset.y}px)`,
        )
      }
    },
    [elementRef],
  )

  useSelectorWithCallback(
    (store) => store.editor.canvas.roundedCanvasOffset,
    (newOffset) => transformCanvasOffset(newOffset),
  )

  transformCanvasOffset(offsetRef.current)
}
