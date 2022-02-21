import React from 'react'
import { CanvasScale, CanvasScrollOffset } from '../../utils/global-positions'
import { useSelectorWithCallback } from '../editor/store/store-hook'

export const UtopiaCanvasVarStyleTag = React.memo(() => {
  const styleTagRef = React.useRef<HTMLStyleElement>(null)

  useSelectorWithCallback(
    (store) => ({
      roundedCanvasOffset: store.editor.canvas.roundedCanvasOffset,
      canvasScale: store.editor.canvas.scale,
    }),
    ({ roundedCanvasOffset, canvasScale }) => {
      // To make sure we are changing these global variables at the exact same time we are changing the css vars, we are mutating all of them here, at the same time
      CanvasScrollOffset.x = roundedCanvasOffset.x
      CanvasScrollOffset.y = roundedCanvasOffset.y
      CanvasScale.current = canvasScale
      if (styleTagRef.current != null) {
        styleTagRef.current.innerHTML = `
        .utopia-canvas-var-container {
          --utopia-canvas-offset-x: ${roundedCanvasOffset.x}px;
          --utopia-canvas-offset-y: ${roundedCanvasOffset.y}px;
        }
      `
      }
    },
    true,
  )

  return <style ref={styleTagRef} />
})
