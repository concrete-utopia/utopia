import * as React from 'react'
import { useControls } from 'leva'
import { useEditorState } from '../../editor/store/store-hook'
import CanvasActions from '../canvas-actions'

export const SelectionColorPicker = (props: any) => {
  const dispatch = useEditorState((store) => {
    return store.dispatch
  }, 'SelectionColorPicker dispatch')

  const colors = useEditorState((store) => {
    return {
      default: store.editor.canvas.controlColors.default,
      focusable: store.editor.canvas.controlColors.focusable,
      isolated: store.editor.canvas.controlColors.isolated,
      childOfIsolated: store.editor.canvas.controlColors.childOfIsolated,
      childFocusable: store.editor.canvas.controlColors.childFocusable,
    }
  }, 'SelectionColorPicker colors')

  const data = useControls({
    default: {
      value: colors.default,
      onChange: (v) => dispatch([CanvasActions.updateControlColor('default', v)]),
    },
    focusable: {
      value: colors.focusable,
      onChange: (v) => dispatch([CanvasActions.updateControlColor('focusable', v)]),
    },
    isolated: {
      value: colors.isolated,
      onChange: (v) => dispatch([CanvasActions.updateControlColor('isolated', v)]),
    },
    childOfIsolated: {
      value: colors.childOfIsolated,
      onChange: (v) => dispatch([CanvasActions.updateControlColor('childOfIsolated', v)]),
    },
    childFocusable: {
      value: colors.childFocusable,
      onChange: (v) => dispatch([CanvasActions.updateControlColor('childFocusable', v)]),
    },
  })

  return <div></div>
}
