import { MetadataUtils } from '../core/model/element-metadata-utils'
import { Either, isRight } from '../core/shared/either'
import {
  ElementInstanceMetadataMap,
  isIntrinsicElement,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
} from '../core/shared/element-template'
import { CanvasPoint } from '../core/shared/math-utils'
import { NodeModules, ElementPath } from '../core/shared/project-file-types'
import * as EP from '../core/shared/element-path'
import * as PP from '../core/shared/property-path'
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
  AllElementProps,
  InternalClipboard,
  TransientFilesState,
} from './editor/store/editor-state'
import {
  toggleBackgroundLayers,
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleStylePropPaths,
} from './inspector/common/css-utils'
import { areAllSelectedElementsNonAbsolute } from './canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import { generateUidWithExistingComponents } from '../core/model/element-template-utils'
import { defaultTransparentViewElement } from './editor/defaults'
import { treatElementAsFragmentLike } from './canvas/canvas-strategies/strategies/fragment-like-helpers'
import { ElementPathTrees } from '../core/shared/element-path-tree'
import { windowToCanvasCoordinates } from './canvas/dom-lookup'
import { WindowMousePositionRaw } from '../utils/global-positions'
import { ElementContextMenuInstance } from './element-context-menu'

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
  selectedViews: Array<ElementPath>
  jsxMetadata: ElementInstanceMetadataMap
  projectContents: ProjectContentTreeRoot
  nodeModules: NodeModules
  transientFilesState: TransientFilesState | null
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
  hiddenInstances: ElementPath[]
  scale: number
  focusedElementPath: ElementPath | null
  allElementProps: AllElementProps
  pathTrees: ElementPathTrees
  openFile: string | null
  internalClipboard: InternalClipboard
  contextMenuInstance: ElementContextMenuInstance
}

export function requireDispatch(dispatch: EditorDispatch | null | undefined): EditorDispatch {
  return Utils.forceNotNull('Dispatch not supplied.', dispatch)
}

export const duplicateElement: ContextMenuItem<CanvasData> = {
  name: 'Duplicate Element',
  shortcut: '⌘D',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([duplicateSelected()], 'everyone')
  },
  enabled: (data) => {
    return data.selectedViews.every((view) => {
      return !EP.isRootElementOfInstance(view)
    })
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
    requireDispatch(dispatch)([EditorActions.cutSelectionToClipboard()], 'noone')
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

export const copyPropertiesMenuItem: ContextMenuItem<CanvasData> = {
  name: 'Copy Properties',
  enabled: true,
  shortcut: '⌥⌘C',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.copyProperties()], 'noone')
  },
}

export const pasteStyle: ContextMenuItem<CanvasData> = {
  name: 'Paste Style',
  enabled: true,
  shortcut: '⌥⌘V',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.pasteProperties('style')], 'noone')
  },
}

export const pasteLayout: ContextMenuItem<CanvasData> = {
  name: 'Paste Layout',
  enabled: true,
  shortcut: '',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.pasteProperties('layout')], 'noone')
  },
}
export const pasteToReplace: ContextMenuItem<CanvasData> = {
  name: 'Paste to Replace',
  enabled: (data) => data.internalClipboard.elements.length !== 0,
  shortcut: '⇧⌘V',
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.pasteToReplace()], 'noone')
  },
}

export const pasteHere: ContextMenuItem<CanvasData> = {
  name: 'Paste Here',
  enabled: (data) => data.internalClipboard.elements.length !== 0,
  shortcut: '',
  isHidden: (data: CanvasData) => {
    return data.contextMenuInstance === 'context-menu-navigator'
  },
  action: (data, dispatch?: EditorDispatch) => {
    if (WindowMousePositionRaw == null) {
      return
    }
    const pointOnCanvas = windowToCanvasCoordinates(
      data.scale,
      data.canvasOffset,
      WindowMousePositionRaw,
    ).canvasPositionRaw
    requireDispatch(dispatch)([EditorActions.pasteHere(pointOnCanvas)], 'noone')
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
        toggleStylePropPath(PP.create('style', 'border'), toggleBorder),
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
        toggleStylePropPath(PP.create('style', 'boxShadow'), toggleShadow),
      ),
    )
    requireDispatch(dispatch)(actions, 'everyone')
  },
}

export const setAsFocusedElement: ContextMenuItem<CanvasData> = {
  name: 'Edit Component',
  enabled: (data) => {
    return data.selectedViews.every((view) => {
      return MetadataUtils.isFocusableComponent(view, data.jsxMetadata)
    })
  },
  isHidden: (data) => {
    return data.selectedViews.every((view) => {
      const isFocused = EP.pathsEqual(data.focusedElementPath, view)
      const elementName = MetadataUtils.getJSXElementNameFromMetadata(data.jsxMetadata, view)
      return isFocused || (elementName != null ? isIntrinsicElement(elementName) : true)
    })
  },
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      const sv = data.selectedViews[0]
      requireDispatch(dispatch)([
        EditorActions.setFocusedElement(sv),
        EditorActions.scrollToElement(sv, true),
      ])
    }
  },
}

export const removeAsFocusedElement: ContextMenuItem<CanvasData> = {
  name: 'Exit Editing Component',
  enabled: (data) => {
    return true
  },
  isHidden: (data) => {
    return data.selectedViews.every((view) => {
      const isFocused = EP.pathsEqual(data.focusedElementPath, view)
      return !isFocused
    })
  },
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      requireDispatch(dispatch)([EditorActions.setFocusedElement(null)])
    }
  },
}

export const scrollToElement: ContextMenuItem<CanvasData> = {
  name: 'Scroll to',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      const sv = data.selectedViews[0]
      requireDispatch(dispatch)([EditorActions.scrollToElement(sv, false)])
    }
  },
}

export const toggleVisibility: ContextMenuItem<CanvasData> = {
  name: 'Toggle Hidden',
  enabled: true,
  shortcut: '⇧⌘H',
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

export const insert: ContextMenuItem<CanvasData> = {
  name: 'Add Element…',
  shortcut: 'A',
  enabled: true,
  action: (data, dispatch) => {
    requireDispatch(dispatch)([
      EditorActions.openFloatingInsertMenu({
        insertMenuMode: 'insert',
        parentPath: null,
        indexPosition: null,
      }),
    ])
  },
}

export const convert: ContextMenuItem<CanvasData> = {
  name: 'Convert Element To…',
  shortcut: 'C',
  enabled: (data) => {
    return data.selectedViews.every((path) => {
      const element = MetadataUtils.findElementByElementPath(data.jsxMetadata, path)
      return element != null && isRight(element.element) && isJSXElementLike(element.element.value)
    })
  },
  action: (data, dispatch) => {
    requireDispatch(dispatch)([EditorActions.openFloatingInsertMenu({ insertMenuMode: 'convert' })])
  },
}

export const group: ContextMenuItem<CanvasData> = {
  name: 'Group Selection',
  shortcut: '⌘G',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)(
      [
        EditorActions.wrapInElement(data.selectedViews, {
          element: defaultTransparentViewElement(
            generateUidWithExistingComponents(data.projectContents),
          ),
          importsToAdd: {},
        }),
      ],
      'everyone',
    )
  },
}

export const unwrap: ContextMenuItem<CanvasData> = {
  name: 'Unwrap',
  shortcut: '⇧⌘G',
  enabled: (data) => {
    return data.selectedViews.some(
      (path) =>
        MetadataUtils.targetSupportsChildren(
          data.projectContents,
          data.jsxMetadata,
          data.nodeModules,
          data.openFile,
          path,
          data.pathTrees,
        ) ||
        treatElementAsFragmentLike(data.jsxMetadata, data.allElementProps, data.pathTrees, path),
    )
  },
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      requireDispatch(dispatch)([EditorActions.unwrapElement(data.selectedViews[0])], 'everyone')
    }
  },
}

export const wrapInPicker: ContextMenuItem<CanvasData> = {
  name: 'Wrap in…',
  shortcut: 'G',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)(
      [EditorActions.openFloatingInsertMenu({ insertMenuMode: 'wrap' })],
      'everyone',
    )
  },
}

export const wrapInView: ContextMenuItem<CanvasData> = {
  name: 'Wrap in div',
  shortcut: '⌘G',
  enabled: true,
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)(
      [
        EditorActions.wrapInElement(data.selectedViews, {
          element: defaultTransparentViewElement(
            generateUidWithExistingComponents(data.projectContents),
          ),
          importsToAdd: {},
        }),
      ],
      'everyone',
    )
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

export const escapeHatch: ContextMenuItem<CanvasData> = {
  name: 'Convert to Absolute Layout',
  enabled: (data) => {
    return areAllSelectedElementsNonAbsolute(data.selectedViews, data.jsxMetadata)
  },
  action: (data, dispatch?: EditorDispatch) => {
    if (data.selectedViews.length > 0) {
      requireDispatch(dispatch)([EditorActions.runEscapeHatch(data.selectedViews)], 'everyone')
    }
  },
}
