import { MetadataUtils } from '../core/model/element-metadata-utils'
import type { FilePathMappings } from '../core/model/project-file-utils'
import type { Either } from '../core/shared/either'
import { isRight } from '../core/shared/either'
import * as EP from '../core/shared/element-path'
import type { ElementPathTrees } from '../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../core/shared/element-template'
import { isIntrinsicElement, isJSXElementLike } from '../core/shared/element-template'
import type { CanvasPoint, WindowPoint } from '../core/shared/math-utils'
import type { ElementPath } from '../core/shared/project-file-types'
import * as PP from '../core/shared/property-path'
import RU from '../utils/react-utils'
import Utils from '../utils/utils'
import type { ProjectContentTreeRoot } from './assets'
import { createPasteToReplacePostActionActions } from './canvas/canvas-strategies/post-action-options/post-action-options'
import {
  PropsPreservedPasteHerePostActionChoice,
  PropsReplacedPasteHerePostActionChoice,
} from './canvas/canvas-strategies/post-action-options/post-action-paste'
import { treatElementAsFragmentLike } from './canvas/canvas-strategies/strategies/fragment-like-helpers'
import { createWrapInGroupActions } from './canvas/canvas-strategies/strategies/group-conversion-helpers'
import { areAllSelectedElementsNonAbsolute } from './canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import { windowToCanvasCoordinates } from './canvas/dom-lookup'
import { setFocus } from './common/actions'
import type { PropertyControlsInfo } from './custom-code/code-file'
import type { EditorDispatch } from './editor/action-types'
import * as EditorActions from './editor/actions/action-creators'
import {
  copySelectionToClipboard,
  duplicateSelected,
  toggleHidden,
} from './editor/actions/action-creators'
import {
  floatingInsertMenuStateSwap,
  type AllElementProps,
  type InternalClipboard,
  type NavigatorEntry,
  type PasteHerePostActionMenuData,
} from './editor/store/editor-state'
import type { ElementContextMenuInstance } from './element-context-menu'
import {
  toggleBackgroundLayers,
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleStylePropPaths,
} from './inspector/common/css-utils'
import {
  type ShowComponentPickerContextMenu,
  type InsertionTarget,
  type ShowComponentPickerContextMenuCallback,
  renderPropTarget,
} from './navigator/navigator-item/component-picker-context-menu'

export interface ContextMenuItem<T> {
  name: string | React.ReactNode
  enabled: boolean | ((data: T) => boolean)
  submenuName?: string | React.ReactNode | null
  shortcut?: string
  isSeparator?: boolean
  isHidden?: (data: T) => boolean
  action: (
    data: T,
    dispatch: EditorDispatch | undefined,
    rightClickCoordinate: WindowPoint | null,
    event: React.MouseEvent | React.TouchEvent | React.KeyboardEvent,
  ) => void
}

export interface CanvasData {
  canvasOffset: CanvasPoint
  selectedViews: Array<ElementPath>
  jsxMetadata: ElementInstanceMetadataMap
  projectContents: ProjectContentTreeRoot
  filePathMappings: FilePathMappings
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
  hiddenInstances: ElementPath[]
  scale: number
  focusedElementPath: ElementPath | null
  allElementProps: AllElementProps
  pathTrees: ElementPathTrees
  internalClipboard: InternalClipboard
  contextMenuInstance: ElementContextMenuInstance
  autoFocusedPaths: Array<ElementPath>
  navigatorTargets: Array<NavigatorEntry>
  propertyControlsInfo: PropertyControlsInfo
  showComponentPicker: ShowComponentPickerContextMenuCallback
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
  enabled: (data) =>
    data.internalClipboard.elements.length !== 0 &&
    data.selectedViews.some((target) => !EP.isRootElementOfInstance(target)),
  shortcut: '⇧⌘V',
  action: (data, dispatch?: EditorDispatch) => {
    const actions = createPasteToReplacePostActionActions(
      data.selectedViews,
      data.internalClipboard,
    )
    if (actions != null) {
      requireDispatch(dispatch)(actions, 'noone')
    }
  },
}

export const pasteHere: ContextMenuItem<CanvasData> = {
  name: 'Paste Here',
  enabled: (data) => data.internalClipboard.elements.length !== 0,
  shortcut: '',
  isHidden: (data: CanvasData) => {
    return data.contextMenuInstance === 'context-menu-navigator'
  },
  action: (data, dispatch: EditorDispatch | undefined, mouseWindowPosition: WindowPoint | null) => {
    if (mouseWindowPosition == null) {
      return
    }
    const pointOnCanvas = windowToCanvasCoordinates(
      data.scale,
      data.canvasOffset,
      mouseWindowPosition,
    ).canvasPositionRaw
    const pasteHerePostActionData = {
      type: 'PASTE_HERE',
      position: pointOnCanvas,
      internalClipboard: data.internalClipboard,
    } as PasteHerePostActionMenuData

    const defaultChoice =
      PropsReplacedPasteHerePostActionChoice(pasteHerePostActionData) ??
      PropsPreservedPasteHerePostActionChoice(pasteHerePostActionData)

    if (defaultChoice != null) {
      requireDispatch(dispatch)(
        [
          EditorActions.startPostActionSession(pasteHerePostActionData),
          EditorActions.executePostActionMenuChoice(defaultChoice),
        ],
        'noone',
      )
    }
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
      return MetadataUtils.isManuallyFocusableComponent(
        view,
        data.jsxMetadata,
        data.autoFocusedPaths,
        data.filePathMappings,
        data.propertyControlsInfo,
        data.projectContents,
      )
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
        EditorActions.scrollToElement(sv, 'keep-scroll-position-if-visible'),
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
      requireDispatch(dispatch)([EditorActions.scrollToElement(sv, 'to-center')])
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
  action: (data, _dispatch, _coord, event) => {
    data.showComponentPicker(data.selectedViews, EditorActions.insertAsChildTarget())(event)
  },
}

export function showWrapComponentPicker(
  selectedViews: ElementPath[],
  jsxMetadata: ElementInstanceMetadataMap,
  showComponentPicker: ShowComponentPickerContextMenuCallback,
): ShowComponentPickerContextMenu {
  return showComponentPicker(selectedViews, EditorActions.wrapTarget)
}

export function showReplaceComponentPicker(
  targetElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  showComponentPicker: ShowComponentPickerContextMenuCallback,
): ShowComponentPickerContextMenu {
  const element = MetadataUtils.findElementByElementPath(jsxMetadata, targetElement)
  const prop = element?.assignedToProp
  const target = prop == null ? targetElement : EP.parentPath(targetElement)
  const insertionTarget: InsertionTarget =
    prop == null ? EditorActions.replaceTarget : renderPropTarget(prop)
  return showComponentPicker([target], insertionTarget)
}

export function showSwapComponentPicker(
  targetElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  showComponentPicker: ShowComponentPickerContextMenuCallback,
): ShowComponentPickerContextMenu {
  const element = MetadataUtils.findElementByElementPath(jsxMetadata, targetElement)
  const prop = element?.assignedToProp
  const target = prop == null ? targetElement : EP.parentPath(targetElement)
  const insertionTarget: InsertionTarget =
    prop == null ? EditorActions.replaceKeepChildrenAndStyleTarget : renderPropTarget(prop)
  return showComponentPicker([target], insertionTarget)
}

export const convert: ContextMenuItem<CanvasData> = {
  name: 'Replace This…',
  shortcut: '',
  enabled: (data) => {
    return (
      data.selectedViews.length > 0 &&
      data.selectedViews.every((path) => {
        const element = MetadataUtils.findElementByElementPath(data.jsxMetadata, path)
        return (
          element != null && isRight(element.element) && isJSXElementLike(element.element.value)
        )
      })
    )
  },
  action: (data, _dispatch, _coord, event) => {
    showSwapComponentPicker(
      data.selectedViews[0],
      data.jsxMetadata,
      data.showComponentPicker,
    )(event)
  },
}

export const replace: ContextMenuItem<CanvasData> = {
  name: 'Replace Everything…',
  shortcut: '',
  enabled: (data) => {
    return (
      data.selectedViews.length > 0 &&
      data.selectedViews.every((path) => {
        const element = MetadataUtils.findElementByElementPath(data.jsxMetadata, path)
        return (
          element != null && isRight(element.element) && isJSXElementLike(element.element.value)
        )
      })
    )
  },
  action: (data, _dispatch, _coord, event) => {
    showReplaceComponentPicker(
      data.selectedViews[0],
      data.jsxMetadata,
      data.showComponentPicker,
    )(event)
  },
}

export const group: ContextMenuItem<CanvasData> = {
  name: 'Group Selection',
  shortcut: '⌘G',
  enabled: true,
  action: (data: CanvasData, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)(
      [
        createWrapInGroupActions(
          data.selectedViews,
          data.projectContents,
          data.jsxMetadata,
          data.allElementProps,
          data.pathTrees,
          data.navigatorTargets,
        ),
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
          path,
          data.pathTrees,
          data.propertyControlsInfo,
        ) ||
        treatElementAsFragmentLike(data.jsxMetadata, data.allElementProps, data.pathTrees, path),
    )
  },
  action: (data, dispatch?: EditorDispatch) => {
    requireDispatch(dispatch)([EditorActions.unwrapElements(data.selectedViews)], 'everyone')
  },
}

export const wrapInPicker: ContextMenuItem<CanvasData> = {
  name: 'Wrap in…',
  shortcut: 'W',
  enabled: true,
  action: (data, dispatch, _coord, event) => {
    // eslint-disable-next-line
    // @ts-ignore
    if (window.openOldWrap === true) {
      requireDispatch(dispatch)(
        [setFocus('canvas'), EditorActions.openFloatingInsertMenu({ insertMenuMode: 'wrap' })],
        'everyone',
      )
    } else {
      showWrapComponentPicker(data.selectedViews, data.jsxMetadata, data.showComponentPicker)(event)
    }
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
      requireDispatch(dispatch)(
        [EditorActions.runEscapeHatch(data.selectedViews, 'set-hugging-parent-to-fixed')],
        'everyone',
      )
    }
  },
}
