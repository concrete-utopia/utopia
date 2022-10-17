import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { EditorState } from '../../../editor/store/editor-state'
import { cursorForMissingReparentedItems } from '../../canvas-strategies/strategies/reparent-utils'
import { CSSCursor } from '../../canvas-types'
import { getCursorFromDragState } from '../../canvas-utils'
import { useDelayedEditorState } from '../../canvas-strategies/canvas-strategies'
import { useEditorState } from '../../../editor/store/store-hook'
import { LeftMenuWidth } from '../../../menubar/menubar'
import { FileBrowserItemProps } from '../../../filebrowser/fileitem'
import { ConnectableElement, DndProvider, DropTargetHookSpec, useDrop } from 'react-dnd'
import { HTML5Backend } from 'react-dnd-html5-backend'

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

  const noImageDragSessionInProgress = useDelayedEditorState(
    (store) => store.editor.imageDragSessionState.type === 'NOT_DRAGGING',
  )

  const leftPaneWidth = useEditorState((store) => {
    return store.editor.leftMenu.paneWidth + LeftMenuWidth
  }, 'CursorOverlay leftPaneWidth')

  const styleProps = React.useMemo(() => {
    let workingStyleProps: React.CSSProperties = {
      position: 'fixed',
      left: noImageDragSessionInProgress ? 0 : leftPaneWidth,
      top: 0,
      width: '100vw',
      height: '100vh',
      pointerEvents: 'none',
      zIndex: 9999999,
    }
    if (cursor != null) {
      workingStyleProps.cursor = cursor
      workingStyleProps.pointerEvents = 'all'
    }
    return workingStyleProps
  }, [cursor, noImageDragSessionInProgress, leftPaneWidth])

  // react-dnd is used here to accept dropping from the filebrowser to the canvas
  const dropSpec: DropTargetHookSpec<FileBrowserItemProps, 'CANVAS', unknown> = {
    accept: 'files',
    canDrop: () => true,
  }
  const [_, drop] = useDrop(dropSpec)
  const forwardedRef = React.useCallback(
    (node: ConnectableElement) => {
      return drop(node)
    },
    [drop],
  )

  const portalDiv = document.getElementById('cursor-overlay-portal')
  if (portalDiv == null) {
    return null
  }
  return ReactDOM.createPortal(
    <DndProvider backend={HTML5Backend}>
      <div ref={forwardedRef} key='cursor-area' id='cursor-overlay' style={styleProps} />
    </DndProvider>,
    portalDiv,
  )
})
