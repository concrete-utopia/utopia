import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { EditorState } from 'src/components/editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { StrategyState } from '../../canvas-strategies/interaction-state'
import { cursorForMissingReparentedItems } from '../../canvas-strategies/reparent-utils'
import { CSSCursor } from '../../canvas-types'
import { getCursorFromDragState } from '../../canvas-utils'

export function getCursorForOverlay(
  editorState: EditorState,
  strategyState: StrategyState,
): CSSCursor | null {
  const forMissingReparentedItems = cursorForMissingReparentedItems(
    strategyState.customStrategyState,
    editorState.spyMetadata,
  )
  return (
    forMissingReparentedItems ?? getCursorFromDragState(editorState) ?? editorState.canvas.cursor
  )
}

export const CursorOverlay = React.memo(() => {
  const cursor = useEditorState((store) => {
    return getCursorForOverlay(store.editor, store.strategyState)
  }, 'CursorOverlay cursor')

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
      workingStyleProps.pointerEvents = 'all'
    }
    return workingStyleProps
  }, [cursor])
  const portalDiv = document.getElementById('cursor-overlay-portal')
  if (portalDiv == null) {
    return null
  }
  return ReactDOM.createPortal(
    <div key='cursor-area' id='cursor-overlay' data-testid='cursor-overlay' style={styleProps} />,
    portalDiv,
  )
})
