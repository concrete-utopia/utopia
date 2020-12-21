import * as React from 'react'
import * as EditorActions from '../../editor/actions/action-creators'
import { Mode } from '../../editor/editor-modes'
import { EditorDispatch } from '../../editor/action-types'
import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { pickSelectionEnabled } from './select-mode/select-mode-hooks'

interface DeselectControlProps {}

export const DeselectControl = betterReactMemo<DeselectControlProps>('DeselectControl', (props) => {
  const editorStateRef = useRefEditorState((store) => store.editor)
  const dispatch = useEditorState((store) => store.dispatch, 'DeselectControl dispatch')

  const onMouseDown = React.useCallback(() => {
    const selectionEnabled = pickSelectionEnabled(
      editorStateRef.current.canvas,
      editorStateRef.current.keysPressed,
    )
    if (selectionEnabled && editorStateRef.current.mode.type !== 'insert') {
      dispatch([EditorActions.clearSelection()], 'canvas')
    }
  }, [dispatch, editorStateRef])

  return (
    <div
      data-testid='deselect-control'
      style={{
        position: 'absolute',
        top: 0,
        left: 0,
        width: '100%',
        height: '100%',
      }}
      onMouseDown={onMouseDown}
    />
  )
})
