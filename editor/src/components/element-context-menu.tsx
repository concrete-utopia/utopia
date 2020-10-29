import * as React from 'react'
import {
  bringForward,
  bringToFront,
  copyElements,
  cutElements,
  duplicateElement,
  group,
  lineSeparator,
  rename,
  sendBackward,
  sendToBack,
  toggleVisibility,
  ungroup,
  wrapInView,
  toggleBackgroundLayersItem,
  toggleBorderItem,
  toggleShadowItem,
  ContextMenuItem,
  CanvasData,
} from './context-menu-items'
import { MomentumContextMenu } from './context-menu-wrapper'
import { useRefEditorState, useEditorState } from './editor/store/store-hook'
import { filterScenes } from '../core/shared/template-path'
import { betterReactMemo } from 'uuiui-deps'

export type ElementContextMenuInstance =
  | 'context-menu-navigator'
  | 'context-menu-canvas'
  | 'context-menu-instance-inspector'

interface ElementContextMenuProps {
  contextMenuInstance: ElementContextMenuInstance
}

const ElementContextMenuItems: Array<ContextMenuItem<CanvasData>> = [
  cutElements,
  copyElements,
  duplicateElement,
  lineSeparator,
  group,
  ungroup,
  wrapInView,
  rename,
  lineSeparator,
  lineSeparator,
  bringForward,
  bringToFront,
  sendBackward,
  sendToBack,
  lineSeparator,
  toggleVisibility,
  lineSeparator,
  toggleBackgroundLayersItem,
  toggleBorderItem,
  toggleShadowItem,
]

// TODO Scene Implementation - seems we should have a different context menu for scenes
export const ElementContextMenu = betterReactMemo(
  'ElementContextMenu',
  ({ contextMenuInstance }: ElementContextMenuProps) => {
    const { dispatch } = useEditorState((store) => {
      return { dispatch: store.dispatch }
    }, 'ElementContextMenu dispatch')

    const editorSliceRef = useRefEditorState((store) => {
      return {
        canvasOffset: store.editor.canvas.roundedCanvasOffset,
        selectedViews: store.editor.selectedViews,
        editorDispatch: store.dispatch,
      }
    })

    const getData = React.useCallback(() => {
      const currentEditor = editorSliceRef.current
      return {
        canvasOffset: currentEditor.canvasOffset,
        selectedViews: filterScenes(currentEditor.selectedViews),
      }
    }, [editorSliceRef])

    return (
      <MomentumContextMenu
        id={contextMenuInstance}
        key='element-context-menu'
        items={ElementContextMenuItems}
        dispatch={dispatch}
        getData={getData}
      />
    )
  },
)
