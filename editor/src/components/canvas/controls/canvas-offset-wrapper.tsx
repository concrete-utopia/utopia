import React from 'react'
import { CanvasVector } from '../../../core/shared/math-utils'
import {
  Substores,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'

export const CanvasOffsetWrapper = React.memo((props: { children?: React.ReactNode }) => {
  const elementRef = useApplyCanvasOffsetToStyle(false)

  return (
    <div ref={elementRef} style={{ position: 'absolute' }}>
      {props.children}
    </div>
  )
})

export function useApplyCanvasOffsetToStyle(setScaleToo: boolean): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const applyCanvasOffset = React.useCallback(
    (roundedCanvasOffset: CanvasVector) => {
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

  useSelectorWithCallback(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    applyCanvasOffset,
    'useApplyCanvasOffsetToStyle',
  )

  const applyCanvasOffsetEffect = React.useCallback(() => {
    applyCanvasOffset(canvasOffsetRef.current)
  }, [applyCanvasOffset, canvasOffsetRef])
  React.useLayoutEffect(applyCanvasOffsetEffect, [applyCanvasOffsetEffect])

  return elementRef
}
