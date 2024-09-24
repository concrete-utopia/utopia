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
  convertToTailwind,
  convertFromTailwind,
} from './context-menu-items'
import { ContextMenu } from './context-menu-wrapper'
import { useRefEditorState, useEditorState, Substores } from './editor/store/store-hook'
import { CanvasContextMenuPortalTargetID } from '../core/shared/utils'
import type { EditorDispatch } from './editor/action-types'
import { setHighlightedView } from './editor/actions/action-creators'
import { selectComponents } from './editor/actions/meta-actions'
import * as EP from '../core/shared/element-path'
import type { ElementPath } from '../core/shared/project-file-types'
import { getNamesAndIconsAllPaths } from './inspector/common/name-and-icon-hook'
import type { IcnProps } from '../uuiui'
import { FlexRow, Icn, useColorTheme } from '../uuiui'
import { getAllTargetsAtPoint } from './canvas/dom-lookup'
import { WindowMousePositionRaw } from '../utils/global-positions'
import type { WindowPoint } from '../core/shared/math-utils'
import { pointsEqual } from '../core/shared/math-utils'
import { useDispatch } from './editor/store/dispatch-context'
import { useCreateCallbackToShowComponentPicker } from './navigator/navigator-item/component-picker-context-menu'
import { navigatorTargetsSelector, useGetNavigatorTargets } from './navigator/navigator-utils'
import { foldEither } from '../core/shared/either'
import type { AllElementProps, EditorStorePatched } from './editor/store/editor-state'
import type { ElementInstanceMetadataMap } from '../core/shared/element-template'
import type { ElementPathTrees } from '../core/shared/element-path-tree'
import type { FilePathMappings } from '../core/model/project-file-utils'
import type { PropertyControlsInfo } from './custom-code/code-file'
import type { ProjectContentTreeRoot } from './assets'

export type ElementContextMenuInstance =
  | 'context-menu-navigator'
  | 'context-menu-canvas'
  | 'context-menu-canvas-no-selection'
  | 'context-menu-add-page'

interface ElementContextMenuProps {
  contextMenuInstance: ElementContextMenuInstance
}

const ElementContextMenuItems: Array<ContextMenuItem<CanvasData>> = [
  convertToTailwind,
  convertFromTailwind,
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

function getCanvasContextMenuItems(
  contextMenuInstance: ElementContextMenuInstance,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  autoFocusedPaths: Array<ElementPath>,
  filePathMappings: FilePathMappings,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
  dispatch: EditorDispatch,
): Array<ContextMenuItem<CanvasData>> {
  const elementNamesAndIcons = getNamesAndIconsAllPaths(
    metadata,
    allElementProps,
    elementPathTree,
    autoFocusedPaths,
    filePathMappings,
    propertyControlsInfo,
    projectContents,
  )

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
                data.autoFocusedPaths,
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
      remixRoutingTable: foldEither(
        () => null,
        (remixData) => remixData?.routingTable,
        store.derived.remixData,
      ),
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
      autofocusedPaths: store.derived.autoFocusedPaths,
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
      autofocusedPaths: currentEditor.autoFocusedPaths,
    }
  }, [editorSliceRef, navigatorTargetsRef, contextMenuInstance, showComponentPicker])
}

export const ElementContextMenu = React.memo(({ contextMenuInstance }: ElementContextMenuProps) => {
  const dispatch = useDispatch()
  const getData: () => CanvasData = useCanvasContextMenuGetData(contextMenuInstance)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const autoFocusedPathsRef = useRefEditorState((store) => store.derived.autoFocusedPaths)
  const filePathMappingsRef = useRefEditorState((store) => store.derived.filePathMappings)
  const propertyControlsInfoRef = useRefEditorState((store) => store.editor.propertyControlsInfo)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const contextMenuItems = React.useCallback(() => {
    return getCanvasContextMenuItems(
      contextMenuInstance,
      metadataRef.current,
      allElementPropsRef.current,
      elementPathTreeRef.current,
      autoFocusedPathsRef.current,
      filePathMappingsRef.current,
      propertyControlsInfoRef.current,
      projectContentsRef.current,
      dispatch,
    )
  }, [
    allElementPropsRef,
    autoFocusedPathsRef,
    contextMenuInstance,
    dispatch,
    elementPathTreeRef,
    filePathMappingsRef,
    metadataRef,
    projectContentsRef,
    propertyControlsInfoRef,
  ])

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
