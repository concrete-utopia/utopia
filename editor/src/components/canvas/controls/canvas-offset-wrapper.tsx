import React from 'react'
import type { CanvasVector } from '../../../core/shared/math-utils'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { isFollowMode } from '../../editor/editor-modes'
import { liveblocksThrottle } from '../../../../liveblocks.config'

export const CanvasOffsetWrapper = React.memo(
  (props: { children?: React.ReactNode; setScaleToo?: boolean }) => {
    const elementRef = useApplyCanvasOffsetToStyle(props.setScaleToo ?? false)

    return (
      <div ref={elementRef} style={{ position: 'absolute' }}>
        {props.children}
      </div>
    )
  },
)

export function useApplyCanvasOffsetToStyle(setScaleToo: boolean): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const isScrollAnimationActiveRef = useRefEditorState(
    (store) => store.editor.canvas.scrollAnimation,
  )

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'useApplyCanvasOffsetToStyle mode',
  )

  const applyCanvasOffset = React.useCallback(
    (roundedCanvasOffset: CanvasVector) => {
      if (elementRef.current != null) {
        elementRef.current.style.setProperty(
          'transform',
          (setScaleToo && scaleRef.current < 1 ? `scale(${scaleRef.current})` : '') +
            ` translate3d(${roundedCanvasOffset.x}px, ${roundedCanvasOffset.y}px, 0)`,
        )
        // elementRef.current.style.setProperty(
        //   'zoom',
        //   setScaleToo && scaleRef.current >= 1 ? `${scaleRef.current * 100}%` : '1',
        // )

        if (!isScrollAnimationActiveRef.current) {
          elementRef.current.style.setProperty(
            'transition',
            isFollowMode(mode) ? `transform ${liveblocksThrottle}ms linear` : 'none',
          )
        }
      }
    },
    [setScaleToo, scaleRef, isScrollAnimationActiveRef, mode],
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
