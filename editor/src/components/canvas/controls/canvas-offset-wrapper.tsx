import React from 'react'
import { CanvasVector } from '../../../core/shared/math-utils'
import { useRefEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'

export const CanvasOffsetWrapper = React.memo((props) => {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const offsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const transformCanvasOffset = React.useCallback(
    (canvasOffset: CanvasVector) => {
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

  return (
    <div ref={elementRef} style={{ position: 'absolute' }}>
      {props.children}
    </div>
  )
})

export function useApplyCanvasOffsetToComponentEntry(
  elementRef: React.RefObject<HTMLDivElement>,
  scale: number | null,
): void {
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
}
