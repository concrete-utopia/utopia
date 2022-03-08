import React from 'react'
import { CanvasVector } from '../../../core/shared/math-utils'
import { useRefEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'

export const CanvasOffsetWrapper = React.memo((props) => {
  const elementRef = useApplyCanvasOffsetToStyle(null)

  return (
    <div ref={elementRef} style={{ position: 'absolute' }}>
      {props.children}
    </div>
  )
})

export function useApplyCanvasOffsetToStyle(scale: number | null): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const applyCanvasOffset = React.useCallback(
    (roundedCanvasOffset: CanvasVector) => {
      if (elementRef.current != null) {
        elementRef.current.style.setProperty(
          'transform',
          (scale != null && scale < 1 ? `scale(${scale})` : '') +
            ` translate3d(${roundedCanvasOffset.x}px, ${roundedCanvasOffset.y}px, 0)`,
        )
        elementRef.current.style.setProperty(
          'zoom',
          scale != null && scale >= 1 ? `${scale * 100}%` : '1',
        )
      }
    },
    [elementRef, scale],
  )

  useSelectorWithCallback((store) => store.editor.canvas.roundedCanvasOffset, applyCanvasOffset)

  const applyCanvasOffsetEffect = React.useCallback(
    () => applyCanvasOffset(canvasOffsetRef.current),
    [applyCanvasOffset, canvasOffsetRef],
  )
  React.useEffect(applyCanvasOffsetEffect, [applyCanvasOffsetEffect])
  return elementRef
}
