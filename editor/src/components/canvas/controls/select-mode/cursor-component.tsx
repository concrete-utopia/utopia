import * as React from 'react'
import { EditorState } from '../../../editor/store/editor-state'
import { cursorForMissingReparentedItems } from '../../canvas-strategies/strategies/reparent-utils'
import { CSSCursor } from '../../canvas-types'
import { getCursorFromDragState } from '../../canvas-utils'
import { useDelayedEditorState } from '../../canvas-strategies/canvas-strategies'

export function getCursorFromEditor(editorState: EditorState): CSSCursor | null {
  const forMissingReparentedItems = cursorForMissingReparentedItems(
    editorState.canvas.controls.reparentedToPaths,
    editorState.spyMetadata,
  )
  return (
    forMissingReparentedItems ?? getCursorFromDragState(editorState) ?? editorState.canvas.cursor
  )
}

export const CursorComponent = React.memo(() => {
  const cursor = useDelayedEditorState((store) => {
    return getCursorFromEditor(store.editor)
  }, 'CursorComponent getCursorFromEditor')

  React.useEffect(() => {
    if (cursor != null) {
      document.body.classList.add('customCursor')
      document.body.style.cursor = cursor
    } else {
      document.body.classList.remove('customCursor')
      document.body.style.cursor = 'unset'
    }
  }, [cursor])

  return (
    <style>{`
      body.customCursor * {
        cursor: inherit !important;
        pointer-events: none;
      };
  `}</style>
  )
})
