import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { Utils } from '../../../../uuiui-deps'
import { useEditorState } from '../../../editor/store/store-hook'
import { cursorForMissingReparentedItems } from '../../canvas-strategies/reparent-utils'
import { getCursorFromDragState } from '../../canvas-utils'

export const CursorOverlay = React.memo(() => {
  const cursor = useEditorState((store) => {
    const forMissingReparentedItems = cursorForMissingReparentedItems(
      store.strategyState.customStrategyState,
      store.editor.spyMetadata,
    )
    return (
      forMissingReparentedItems ??
      store.editor.canvas.cursor ??
      getCursorFromDragState(store.editor)
    )
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
    <div key='cursor-area' id='cursor-overlay' style={styleProps} />,
    portalDiv,
  )
})
