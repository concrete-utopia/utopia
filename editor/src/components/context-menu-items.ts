import * as R from 'ramda'
import { TriggerEvent } from 'react-contexify'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { Either } from '../core/shared/either'
import { ElementInstanceMetadataMap, isIntrinsicHTMLElement } from '../core/shared/element-template'
import { CanvasPoint } from '../core/shared/math-utils'
import {
  InstancePath,
  NodeModules,
  ScenePath,
  TemplatePath,
} from '../core/shared/project-file-types'
import * as PP from '../core/shared/property-path'
import * as TP from '../core/shared/template-path'
import RU from '../utils/react-utils'
import Utils from '../utils/utils'
import { ProjectContentTreeRoot } from './assets'
import { EditorDispatch } from './editor/action-types'
import * as EditorActions from './editor/actions/action-creators'
import {
  copySelectionToClipboard,
  deleteView,
  duplicateSelected,
  toggleHidden,
} from './editor/actions/action-creators'
import {
  getJSXComponentsAndImportsForPathInnerComponent,
  TransientFilesState,
} from './editor/store/editor-state'
import {
  toggleBackgroundLayers,
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleStylePropPaths,
} from './inspector/common/css-utils'

export interface ContextMenuItem<T> {
  name: string | React.ReactNode
  enabled: boolean | ((data: T) => boolean)
  submenuName?: string | null
  shortcut?: string
  isSeparator?: boolean
  isHidden?: (data: T) => boolean
  action: (data: T, dispatch?: EditorDispatch, event?: MouseEvent) => void
}

export interface CanvasData {
  canvasOffset: CanvasPoint
  selectedViews: Array<InstancePath>
  jsxMetadata: ElementInstanceMetadataMap
  currentFilePath: string | null
  projectContents: ProjectContentTreeRoot
  nodeModules: NodeModules
  transientFilesState: TransientFilesState | null
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
  focusedElementPath: ScenePath | null
  hiddenInstances: TemplatePath[]
  scale: number
}

export function requireDispatch(dispatch: EditorDispatch | null | undefined): EditorDispatch {
  return Utils.forceNotNull('Dispatch not supplied.', dispatch)
}

export const duplicateElement: ContextMenuItem<CanvasData> = {
  name: 'Duplicate Element',
  enabled: true,
  shortcut: '⌘D',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([duplicateSelected()], 'everyone')
  },
}

export const copyElements: ContextMenuItem<CanvasData> = {
  name: 'Copy',
  enabled: true,
  shortcut: '⌘C',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([copySelectionToClipboard()], 'noone')
  },
}

export const cutElements: ContextMenuItem<CanvasData> = {
  name: 'Cut',
  enabled: true,
  shortcut: '⌘X',
  action: (data, dispatch?: EditorDispatch) => {
    const deleteTargets = data.selectedViews
    const deleteActions = deleteTargets.map(deleteView)
    requireDispatch(dispatch)([copySelectionToClipboard(), ...deleteActions], 'noone')
  },
}

export const pasteElements: ContextMenuItem<CanvasData> = {
  name: 'Paste',
  enabled: false,
  shortcut: '⌘V',
  action: (data, dispatch?: EditorDispatch) => {
    // triggering a paste from a user event that is _not_ the paste event (like a mouse click in this instance)
    // the standard process can be tracked from here: https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/read
    // right now, only Firefox supports Clipboard.read() yet
    // Clipboard.readText() already works in Chrome 65+,
    // but readText is not enough for us, as we don't want to store our model in user-readable and easily user-editable
    // plaintext clipboard. that would lead to a subpar experience for everyone involved
  },
}

export const toggleBackgroundLayersItem: ContextMenuItem<CanvasData> = {
  name: 'Toggle Fill',
  enabled: true,
  shortcut: 'F',
  action: (data, dispatch?: EditorDispatch) => {
    const actions = data.selectedViews.map((target) =>
      EditorActions.toggleProperty(target, toggleStylePropPaths(toggleBackgroundLayers)),
    )
    requireDispatch(dispatch)(actions, 'everyone')
  },
}

export const toggleBorderItem: ContextMenuItem<CanvasData> = {
  name: 'Toggle Border',
  enabled: true,
  shortcut: 'B',
  action: (data, dispatch?: EditorDispatch) => {
    const actions = data.selectedViews.map((target) =>
      EditorActions.toggleProperty(
        target,
        toggleStylePropPath(PP.create(['style', 'border']), toggleBorder),
      ),
    )
    requireDispatch(dispatch)(actions, 'everyone')
  },
}

export const toggleShadowItem: ContextMenuItem<CanvasData> = {
  name: 'Toggle Shadow',
  enabled: true,
  shortcut: 'S',
  action: (data, dispatch?: EditorDispatch) => {
    const actions = data.selectedViews.map((target) =>
      EditorActions.toggleProperty(
        target,
        toggleStylePropPath(PP.create(['style', 'boxShadow']), toggleShadow),
      ),
    )
    requireDispatch(dispatch)(actions, 'everyone')
  },
}

export const setAsFocusedElement: ContextMenuItem<CanvasData> = {
  name: 'Edit Component',
  enabled: (data) => {
    if (data.currentFilePath == null) {
      return false
    } else {
      return data.selectedViews.every((view) => {
        const { components, imports } = getJSXComponentsAndImportsForPathInnerComponent(
          view,
          data.currentFilePath,
          data.projectContents,
          data.nodeModules,
          data.transientFilesState,
          data.resolve,
        )
        return MetadataUtils.isFocusableComponent(view, components, data.jsxMetadata, imports)
      })
    }
  },
  isHidden: (data) => {
    return data.selectedViews.every((view) => {
      const { components } = getJSXComponentsAndImportsForPathInnerComponent(
        view,
        data.currentFilePath,
        data.projectContents,
        data.nodeModules,
        data.transientFilesState,
        data.resolve,
      )
      const elementName = MetadataUtils.getJSXElementName(view, components)
      return elementName != null ? isIntrinsicHTMLElement(elementName) : true
    })
  },
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      const sv = data.selectedViews[0]
      requireDispatch(dispatch)([
        EditorActions.setFocusedElement(TP.scenePathForElementAtInstancePath(sv)),
        EditorActions.scrollToElement(TP.scenePathForElementAtInstancePath(sv), true),
      ])
    }
  },
}

export const scrollToElement: ContextMenuItem<CanvasData> = {
  name: 'Scroll to',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      const sv = data.selectedViews[0]
      requireDispatch(dispatch)([
        EditorActions.scrollToElement(TP.scenePathForElementAtInstancePath(sv), false),
      ])
    }
  },
}

export const toggleVisibility: ContextMenuItem<CanvasData> = {
  name: 'Toggle Hidden',
  enabled: true,
  shortcut: '⌘⇧H',
  action: (_, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([toggleHidden()], 'everyone')
  },
}

export const lineSeparator: ContextMenuItem<unknown> = {
  name: RU.create('div', { key: 'separator', className: 'react-contexify__separator' }, ''),
  enabled: false,
  isSeparator: true,
  action: () => null,
}

export const resetPins: ContextMenuItem<unknown> = {
  name: 'Reset positioning',
  enabled: true,
  action: (data: any, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)(data.targets.map(EditorActions.resetPins), 'noone')
  },
}

export const group: ContextMenuItem<CanvasData> = {
  name: 'Group Selection',
  shortcut: '⌘G',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.wrapInGroup(data.selectedViews)], 'everyone')
  },
}

export const ungroup: ContextMenuItem<CanvasData> = {
  name: 'Ungroup',
  shortcut: '⌘⇧G',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      requireDispatch(dispatch)(
        [EditorActions.unwrapGroupOrView(data.selectedViews[0])],
        'everyone',
      )
    }
  },
}

export const wrapInView: ContextMenuItem<CanvasData> = {
  name: 'Wrap in View',
  shortcut: '⌘⏎',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.wrapInView(data.selectedViews)], 'everyone')
  },
}

export const bringForward: ContextMenuItem<CanvasData> = {
  name: 'Bring Forward',
  shortcut: '⌘]',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.moveSelectedForward()], 'everyone')
  },
}

// TODO This and send to back are broken
export const bringToFront: ContextMenuItem<CanvasData> = {
  name: 'Bring To Front',
  shortcut: '⌘⌥]',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.moveSelectedToFront()], 'everyone')
  },
}

export const sendBackward: ContextMenuItem<CanvasData> = {
  name: 'Send Backward',
  shortcut: '⌘[',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.moveSelectedBackward()], 'everyone')
  },
}

export const sendToBack: ContextMenuItem<CanvasData> = {
  name: 'Send To Back',
  shortcut: '⌘⌥[',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.moveSelectedToBack()], 'everyone')
  },
}

export const rename: ContextMenuItem<CanvasData> = {
  name: 'Rename',
  shortcut: '⌘R',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      requireDispatch(dispatch)(
        [EditorActions.setNavigatorRenamingTarget(data.selectedViews[0])],
        'everyone',
      )
    }
  },
}
