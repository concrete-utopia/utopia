import * as React from 'react'
import { EditorState } from '../../../editor/store/editor-state'
import { cursorForMissingReparentedItems } from '../../canvas-strategies/strategies/reparent-utils'
import { CSSCursor } from '../../canvas-types'
import { getCursorFromDragState } from '../../canvas-utils'
import { useDelayedEditorState } from '../../canvas-strategies/canvas-strategies'
import { EditorID } from '../../../../core/shared/utils'

export function getCursorFromEditor(editorState: EditorState): CSSCursor | null {
  const forMissingReparentedItems = cursorForMissingReparentedItems(
    editorState.canvas.controls.reparentedToPaths,
    editorState.spyMetadata,
  )
  return (
    forMissingReparentedItems ?? getCursorFromDragState(editorState) ?? editorState.canvas.cursor
  )
}

export const CursorOverlay = React.memo(() => {
  const cursor = useDelayedEditorState((store) => {
    return getCursorFromEditor(store.editor)
  })

  React.useEffect(() => {
    const editor = document.getElementById(EditorID)
    if (cursor != null) {
      document.body.classList.add('customCursor')
      document.body.style.cursor = cursor
    } else {
      document.body.classList.remove('customCursor')
      document.body.style.cursor = 'unset'
    }

    if (editor != null) {
      editor.style.pointerEvents = 'initial'
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
