import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { EditorState } from '../../../editor/store/editor-state'
import { cursorForMissingReparentedItems } from '../../canvas-strategies/strategies/reparent-utils'
import { CSSCursor } from '../../canvas-types'
import { getCursorFromDragState } from '../../canvas-utils'
import { useDelayedEditorState } from '../../canvas-strategies/canvas-strategies'

export function getCursorForOverlay(editorState: EditorState): CSSCursor | null {
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
    return getCursorForOverlay(store.editor)
  })

  const styleProps = React.useMemo(() => {
    let workingStyleProps: React.CSSProperties = {
      position: 'fixed',
      left: 0,
      top: 0,
      width: '100vw',
      height: '100vh',
      pointerEvents: 'none',
      zIndex: 9999999,
    }
    if (cursor != null) {
      workingStyleProps.cursor = cursor
    }
    return workingStyleProps
  }, [cursor])
  const portalDiv = document.getElementById('cursor-overlay-portal')
  if (portalDiv == null) {
    return null
  }
  return ReactDOM.createPortal(
    <div key='cursor-area' id='cursor-overlay' style={styleProps} />,
    portalDiv,
  )
})
