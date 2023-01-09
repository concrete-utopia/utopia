import React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'
import { TextEditorSpanId } from '../../../text-editor/text-editor'

export const TextEditCanvasOverlay = React.memo(() => {
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'TextEditCanvasOverlay canvas scale',
  )

  const [enabled, setEnabled] = React.useState(false)

  const checkMousePosition = React.useCallback((event: MouseEvent) => {
    const textEditor = document.getElementById(TextEditorSpanId)
    const textEditorBounds = textEditor?.getBoundingClientRect()
    if (textEditorBounds == null) {
      setEnabled(false)
    } else if (
      event.clientX >= textEditorBounds.left &&
      event.clientX <= textEditorBounds.right &&
      event.clientY >= textEditorBounds.top &&
      event.clientY <= textEditorBounds.bottom
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
      }}
    />
  )
})
TextEditCanvasOverlay.displayName = 'TextEditCanvasOverlay'
