import React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'
import { TextEditorSpanId } from '../../../text-editor/text-editor'

export const TextEditCanvasOverlay = React.memo(() => {
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'TextEditCanvasOverlay canvas scale',
  )

  const textEditor = document.getElementById(TextEditorSpanId)
  const textEditorBounds = textEditor?.getBoundingClientRect()

  const [visible, setVisible] = React.useState(false)

  const checkMousePosition = React.useCallback(
    (event: MouseEvent) => {
      if (textEditorBounds == null) {
        setVisible(false)
      } else if (
        event.clientX >= textEditorBounds.left &&
        event.clientX <= textEditorBounds.right &&
        event.clientY >= textEditorBounds.top &&
        event.clientY <= textEditorBounds.bottom
      ) {
        setVisible(false)
      } else {
        setVisible(true)
      }
    },
    [textEditorBounds],
  )

  React.useEffect(() => {
    window.addEventListener('mousemove', (event) => checkMousePosition(event))
    return () => window.removeEventListener('mousemove', checkMousePosition)
  }, [checkMousePosition])

  if (!visible) {
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
