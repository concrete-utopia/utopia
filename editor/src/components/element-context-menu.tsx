import React from 'react'
import * as ReactDOM from 'react-dom'
import type { ContextMenuItem, CanvasData } from './context-menu-items'
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
  unwrap,
  wrapInPicker,
  toggleBackgroundLayersItem,
  toggleBorderItem,
  toggleShadowItem,
  setAsFocusedElement,
  scrollToElement,
  insert,
  convert,
  removeAsFocusedElement,
  escapeHatch,
  pasteStyle,
  pasteLayout,
  copyPropertiesMenuItem,
  pasteToReplace,
  pasteHere,
  replace,
  toggleCanCondense,
} from './context-menu-items'
import { ContextMenu } from './context-menu-wrapper'
import { useRefEditorState, useEditorState, Substores } from './editor/store/store-hook'
import { CanvasContextMenuPortalTargetID } from '../core/shared/utils'
import type { EditorDispatch } from './editor/action-types'
import { setHighlightedView } from './editor/actions/action-creators'
import { selectComponents } from './editor/actions/meta-actions'
import * as EP from '../core/shared/element-path'
import type { ElementPath } from '../core/shared/project-file-types'
import { useNamesAndIconsAllPaths } from './inspector/common/name-and-icon-hook'
import type { IcnProps } from '../uuiui'
import { FlexRow, Icn, useColorTheme } from '../uuiui'
import { getAllTargetsAtPoint } from './canvas/dom-lookup'
import { WindowMousePositionRaw } from '../utils/global-positions'
import type { WindowPoint } from '../core/shared/math-utils'
import { pointsEqual } from '../core/shared/math-utils'
import { useDispatch } from './editor/store/dispatch-context'
import { useCreateCallbackToShowComponentPicker } from './navigator/navigator-item/component-picker-context-menu'
import { navigatorTargetsSelector, useGetNavigatorTargets } from './navigator/navigator-utils'

export type ElementContextMenuInstance =
  | 'context-menu-navigator'
  | 'context-menu-canvas'
  | 'context-menu-canvas-no-selection'
  | 'context-menu-add-page'

interface ElementContextMenuProps {
  contextMenuInstance: ElementContextMenuInstance
}

const ElementContextMenuItems: Array<ContextMenuItem<CanvasData>> = [
  setAsFocusedElement,
  removeAsFocusedElement,
  scrollToElement,
  lineSeparator,
  cutElements,
  copyElements,
  copyPropertiesMenuItem,
  pasteStyle,
  pasteLayout,
  pasteToReplace,
  pasteHere,
  duplicateElement,
  lineSeparator,
  insert,
  lineSeparator,
  convert,
  replace,
  escapeHatch,
  lineSeparator,
  wrapInPicker,
  group,
  unwrap,
  rename,
  lineSeparator,
  bringForward,
  bringToFront,
  sendBackward,
  sendToBack,
  lineSeparator,
  toggleVisibility,
  toggleCanCondense,
  lineSeparator,
  toggleBackgroundLayersItem,
  toggleBorderItem,
  toggleShadowItem,
]

const ContextMenuItemsNoSelection: Array<ContextMenuItem<CanvasData>> = [pasteHere]

function useCanvasContextMenuItems(
  contextMenuInstance: ElementContextMenuInstance,
  dispatch: EditorDispatch,
): Array<ContextMenuItem<CanvasData>> {
  const elementNamesAndIcons = useNamesAndIconsAllPaths()

  if (contextMenuInstance === 'context-menu-canvas-no-selection') {
    return ContextMenuItemsNoSelection
  } else if (contextMenuInstance === 'context-menu-canvas') {
    let elementsUnderCursor: Array<ElementPath> = []
    let lastMousePosition: WindowPoint | null = null
    const elementListSubmenu: Array<ContextMenuItem<CanvasData>> = elementNamesAndIcons.map(
      ({ label, path, iconProps }) => {
        return {
          name: (
            <SelectableElementItem
              path={path}
              dispatch={dispatch}
              iconProps={iconProps}
              label={label}
            />
          ),
          details: {
            path: path,
          },
          submenuName: 'Select Elements',
          enabled: true,
          action: () => dispatch(selectComponents([path], false), 'canvas'),
          isHidden: (data: CanvasData) => {
            // Only run this once as the values used are the same for each and every
            // entry and `getAllTargetsAtPoint` is very expensive.
            if (
              lastMousePosition == null ||
              WindowMousePositionRaw == null ||
              !pointsEqual(lastMousePosition, WindowMousePositionRaw)
            ) {
              elementsUnderCursor = getAllTargetsAtPoint(
                'no-filter',
                WindowMousePositionRaw,
                data.scale,
                data.canvasOffset,
                data.jsxMetadata,
                data.lockedElements,
              )
              lastMousePosition = WindowMousePositionRaw
            }
            return !elementsUnderCursor.some((underCursor: ElementPath) =>
              EP.pathsEqual(underCursor, path),
            )
          },
        }
      },
    )
    return [...elementListSubmenu, ...ElementContextMenuItems]
  } else {
    return ElementContextMenuItems
  }
}

interface SelectableElementItemProps {
  dispatch: EditorDispatch
  path: ElementPath
  iconProps: IcnProps
  label: string
}

const SelectableElementItem = (props: SelectableElementItemProps) => {
  const rawRef = React.useRef<HTMLDivElement>(null)
  const { dispatch, path, iconProps, label } = props

  return (
    <FlexRow ref={rawRef}>
      <Icn {...iconProps} color={'white'} />
      <span style={{ paddingLeft: 6 }}>{label}</span>
    </FlexRow>
  )
}

function useCanvasContextMenuGetData(
  contextMenuInstance: ElementContextMenuInstance,
): () => CanvasData {
  const editorSliceRef = useRefEditorState((store) => {
    const resolveFn = store.editor.codeResultCache.curriedResolveFn(store.editor.projectContents)
    return {
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      selectedViews: store.editor.selectedViews,
      jsxMetadata: store.editor.jsxMetadata,
      projectContents: store.editor.projectContents,
      filePathMappings: store.derived.filePathMappings,
      nodeModules: store.editor.nodeModules.files,
      remixRoutingTable: store.derived.remixData?.routingTable ?? null,
      resolve: resolveFn,
      hiddenInstances: store.editor.hiddenInstances,
      scale: store.editor.canvas.scale,
      focusedElementPath: store.editor.focusedElementPath,
      allElementProps: store.editor.allElementProps,
      pathTrees: store.editor.elementPathTree,
      openFile: store.editor.canvas.openFile?.filename ?? null,
      internalClipboard: store.editor.internalClipboard,
      autoFocusedPaths: store.derived.autoFocusedPaths,
      propertyControlsInfo: store.editor.propertyControlsInfo,
      lockedElements: store.editor.lockedElements,
    }
  })
  const navigatorTargetsRef = useRefEditorState(navigatorTargetsSelector)
  const showComponentPicker = useCreateCallbackToShowComponentPicker()

  return React.useCallback(() => {
    const currentEditor = editorSliceRef.current
    return {
      canvasOffset: currentEditor.canvasOffset,
      selectedViews: currentEditor.selectedViews,
      jsxMetadata: currentEditor.jsxMetadata,
      projectContents: currentEditor.projectContents,
      filePathMappings: currentEditor.filePathMappings,
      nodeModules: currentEditor.nodeModules,
      remixRoutingTable: currentEditor.remixRoutingTable,
      resolve: currentEditor.resolve,
      hiddenInstances: currentEditor.hiddenInstances,
      scale: currentEditor.scale,
      focusedElementPath: currentEditor.focusedElementPath,
      allElementProps: currentEditor.allElementProps,
      pathTrees: currentEditor.pathTrees,
      openFile: currentEditor.openFile,
      internalClipboard: currentEditor.internalClipboard,
      contextMenuInstance: contextMenuInstance,
      autoFocusedPaths: currentEditor.autoFocusedPaths,
      navigatorTargets: navigatorTargetsRef.current.navigatorTargets,
      propertyControlsInfo: currentEditor.propertyControlsInfo,
      showComponentPicker: showComponentPicker,
      lockedElements: currentEditor.lockedElements,
    }
  }, [editorSliceRef, navigatorTargetsRef, contextMenuInstance, showComponentPicker])
}

export const ElementContextMenu = React.memo(({ contextMenuInstance }: ElementContextMenuProps) => {
  const dispatch = useDispatch()

  const getData: () => CanvasData = useCanvasContextMenuGetData(contextMenuInstance)
  const contextMenuItems = useCanvasContextMenuItems(contextMenuInstance, dispatch)

  const portalTarget = document.getElementById(CanvasContextMenuPortalTargetID)
  if (portalTarget == null) {
    return null
  } else {
    return ReactDOM.createPortal(
      <ContextMenu
        id={contextMenuInstance}
        key='element-context-menu'
        items={contextMenuItems}
        dispatch={dispatch}
        getData={getData}
      />,
      portalTarget,
    )
  }
})
