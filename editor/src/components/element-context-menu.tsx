import * as React from 'react'
import * as ReactDOM from 'react-dom'
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
import { ContextMenuInnerProps, MomentumContextMenu } from './context-menu-wrapper'
import { useRefEditorState, useEditorState } from './editor/store/store-hook'
import { filterScenes } from '../core/shared/template-path'
import { betterReactMemo, Utils } from '../uuiui-deps'
import { CanvasContextMenuPortalTargetID } from '../core/shared/utils'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { getOpenUtopiaJSXComponentsFromState } from './editor/store/editor-state'

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

    let contextMenuItems = ElementContextMenuItems
    if (contextMenuInstance === 'context-menu-canvas') {
      const allPaths = useEditorState((store) => store.derived.navigatorTargets, 'ElementContextMenu allPaths')
      const metadata = useEditorState((store) => store.editor.jsxMetadataKILLME, 'ElementContextMenu metadata')
      const components = useEditorState((store) => getOpenUtopiaJSXComponentsFromState(store.editor), 'ElementContextMenu components')
      
      const elementListSubmenu: Array<ContextMenuItem<CanvasData>> = allPaths.map(path => {
        const elementName = MetadataUtils.getJSXElementName(path, components, metadata.components)
        return {
          name: elementName?.baseVariable || '',
          details: {
            path: path,
          },
          submenuName: 'Elements',
          enabled: true,
          action: Utils.NO_OP,
        }
      })
      contextMenuItems.push(...elementListSubmenu)
    }

    const portalTarget = document.getElementById(CanvasContextMenuPortalTargetID)
    if (portalTarget == null) {
      return null
    } else {
      return ReactDOM.createPortal((
        <MomentumContextMenu
          id={contextMenuInstance}
          key='element-context-menu'
          items={contextMenuItems}
          dispatch={dispatch}
          getData={getData}
        />
      ), portalTarget)
    }
  },
)
