import React from 'react'
import { CanvasVector } from '../../../core/shared/math-utils'
import { useRefEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'

export const CanvasOffsetWrapper = React.memo((props) => {
  const elementRef = useApplyCanvasOffset(null)

  return (
    <div ref={elementRef} style={{ position: 'absolute' }}>
      {props.children}
    </div>
  )
})

export function useApplyCanvasOffset(scale: number | null): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const applyCanvasOffset = React.useCallback(
    (roundedCanvasOffset: CanvasVector) => {
      if (elementRef.current != null && scale != null) {
        elementRef.current.style.setProperty(
          'transform',
          (scale < 1 ? `scale(${scale})` : '') +
            ` translate3d(${roundedCanvasOffset.x}px, ${roundedCanvasOffset.y}px, 0)`,
        )
      }
    },
    [elementRef, scale],
  )

  useSelectorWithCallback((store) => store.editor.canvas.roundedCanvasOffset, applyCanvasOffset)
  applyCanvasOffset(canvasOffsetRef.current)
  return elementRef
}
