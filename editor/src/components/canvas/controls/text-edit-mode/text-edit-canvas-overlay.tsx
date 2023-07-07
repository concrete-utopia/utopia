import React from 'react'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { TextEditorSpanId } from '../../../text-editor/text-editor'
import type { CSSCursor } from '../../canvas-types'

interface TextEditCanvasOverlayProps {
  cursor: CSSCursor
}

export const TextEditCanvasOverlay = React.memo((props: TextEditCanvasOverlayProps) => {
  const scale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'TextEditCanvasOverlay canvas scale',
  )

  const [enabled, setEnabled] = React.useState(false)

  const checkMousePosition = React.useCallback((event: MouseEvent) => {
    const textEditor = document.getElementById(TextEditorSpanId)
    if (textEditor == null) {
      setEnabled(false)
      return
    }
    const range = document.createRange()
    range.selectNode(textEditor)
    const rangeBounding = range.getBoundingClientRect()
    const textEditorBounding = textEditor.getBoundingClientRect()

    if (
      event.clientX >= Math.min(rangeBounding.left, textEditorBounding.left) &&
      event.clientX <= Math.max(rangeBounding.right, textEditorBounding.right) &&
      event.clientY >= Math.min(rangeBounding.top, textEditorBounding.top) &&
      event.clientY <= Math.max(rangeBounding.bottom, textEditorBounding.bottom)
    ) {
      setEnabled(false)
    } else {
      setEnabled(true)
    }
  }, [])

  React.useEffect(() => {
    window.addEventListener('mousemove', (event) => checkMousePosition(event))
    return () => window.removeEventListener('mousemove', checkMousePosition)
  }, [checkMousePosition])

  if (!enabled) {
    return null
  }

  return (
    <div
      style={{
        position: 'absolute',
        top: 0,
        left: 0,
        width: `${scale < 1 ? 100 / scale : 100}%`,
        height: `${scale < 1 ? 100 / scale : 100}%`,
        transformOrigin: 'top left',
        transform: scale < 1 ? `scale(${scale}) ` : '',
        cursor: props.cursor,
      }}
    />
  )
})
TextEditCanvasOverlay.displayName = 'TextEditCanvasOverlay'
