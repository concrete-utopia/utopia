import React from 'react'
import { CanvasVector } from '../../../core/shared/math-utils'
import { useRefEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'

export const CanvasOffsetWrapper = React.memo((props) => {
  const elementRef = useApplyCanvasOffsetToStyle(false)

  return (
    <div ref={elementRef} style={{ position: 'absolute' }}>
      {props.children}
    </div>
  )
})

export function useApplyCanvasOffsetToStyle(
  setScaleToo: boolean,
  forceOffset?: boolean, // this is not so nice, but the element is optionally rendered in canvas-component-entry
): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const canvasOffsetRef = useRefEditorState(
    React.useCallback((store) => store.editor.canvas.roundedCanvasOffset, []),
  )
  const scaleRef = useRefEditorState(React.useCallback((store) => store.editor.canvas.scale, []))
  const applyCanvasOffset = React.useCallback(
    (roundedCanvasOffset: CanvasVector, _?: any) => {
      if (elementRef.current != null) {
        elementRef.current.style.setProperty(
          'transform',
          (setScaleToo && scaleRef.current < 1 ? `scale(${scaleRef.current})` : '') +
            ` translate3d(${roundedCanvasOffset.x}px, ${roundedCanvasOffset.y}px, 0)`,
        )
        elementRef.current.style.setProperty(
          'zoom',
          setScaleToo && scaleRef.current >= 1 ? `${scaleRef.current * 100}%` : '1',
        )
      }
    },
    [elementRef, setScaleToo, scaleRef],
  )

  useSelectorWithCallback((store) => store.editor.canvas.roundedCanvasOffset, applyCanvasOffset)

  const applyCanvasOffsetEffect = React.useCallback(() => {
    applyCanvasOffset(canvasOffsetRef.current, forceOffset)
  }, [applyCanvasOffset, canvasOffsetRef, forceOffset])
  React.useLayoutEffect(applyCanvasOffsetEffect, [applyCanvasOffsetEffect])
  return elementRef
}
