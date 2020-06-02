import * as React from 'react'
import { MomentumContextMenu } from '../../context-menu-wrapper'
import { useRefEditorState, useEditorState } from '../../editor/store/store-hook'
import { betterReactMemo } from 'uuiui-deps'
import { ContextMenuItem, requireDispatch } from '../../context-menu-items'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/actions'
import { TemplatePath } from '../../../core/shared/project-file-types'
import utils from '../../../utils/utils'
import * as PP from '../../../core/shared/property-path'

export type InstanceContextMenuInstance = 'context-menu-instance-inspector'

interface InstanceContextMenuProps {
  propNames: string[]
  contextMenuInstance: InstanceContextMenuInstance
}

interface ContextMenuData {
  selectedViews: TemplatePath[]
}

const resetStyle = (enabled: boolean): ContextMenuItem<ContextMenuData> => ({
  name: 'Reset Style',
  enabled: enabled,
  action: (data, dispatch?: EditorDispatch) => {
    utils.fastForEach(data.selectedViews, (view) => {
      requireDispatch(dispatch)(
        [EditorActions.resetPropToDefault(view, PP.create(['style']))],
        'everyone',
      )
    })
  },
})

const resetCSS = (enabled: boolean): ContextMenuItem<ContextMenuData> => ({
  name: 'Reset CSS',
  enabled: enabled,
  action: (data, dispatch?: EditorDispatch) => {
    utils.fastForEach(data.selectedViews, (view) => {
      requireDispatch(dispatch)(
        [EditorActions.resetPropToDefault(view, PP.create(['css']))],
        'everyone',
      )
    })
  },
})

const resetAllProps: ContextMenuItem<ContextMenuData> = {
  name: 'Reset All to Default Value',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    utils.fastForEach(data.selectedViews, (view) => {
      requireDispatch(dispatch)([EditorActions.resetPropToDefault(view, null)], 'everyone')
    })
  },
}

const InstanceContextMenuItems = (propNames: string[]): Array<ContextMenuItem<ContextMenuData>> => {
  return [
    resetStyle(propNames.includes('style')),
    resetCSS(propNames.includes('css')),
    resetAllProps,
  ]
}

// TODO Scene Implementation - seems we should have a different context menu for scenes
export const InstanceContextMenu = betterReactMemo(
  'InstanceContextMenu',
  ({ contextMenuInstance, propNames }: InstanceContextMenuProps) => {
    const { dispatch } = useEditorState((store) => {
      return { dispatch: store.dispatch }
    })

    const editorSliceRef = useRefEditorState((store) => {
      return {
        selectedViews: store.editor.selectedViews,
        editorDispatch: store.dispatch,
      }
    })

    const getData = React.useCallback(() => {
      const currentEditor = editorSliceRef.current
      return {
        selectedViews: currentEditor.selectedViews,
      }
    }, [editorSliceRef])

    return (
      <MomentumContextMenu
        id={contextMenuInstance}
        key='element-context-menu'
        items={InstanceContextMenuItems(propNames)}
        dispatch={dispatch}
        getData={getData}
      />
    )
  },
)
