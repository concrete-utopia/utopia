import React from 'react'
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
  unwrap,
  wrapInView,
  wrapInPicker,
  toggleBackgroundLayersItem,
  toggleBorderItem,
  toggleShadowItem,
  ContextMenuItem,
  CanvasData,
  setAsFocusedElement,
  scrollToElement,
  insert,
  convert,
  removeAsFocusedElement,
} from './context-menu-items'
import { MomentumContextMenu } from './context-menu-wrapper'
import { useRefEditorState, useEditorState } from './editor/store/store-hook'
import { CanvasContextMenuPortalTargetID } from '../core/shared/utils'
import { EditorDispatch } from './editor/action-types'
import { selectComponents, setHighlightedView } from './editor/actions/action-creators'
import * as EP from '../core/shared/element-path'
import { ElementPath } from '../core/shared/project-file-types'
import { useNamesAndIconsAllPaths } from './inspector/common/name-and-icon-hook'
import { FlexRow, Icn, IcnProps, useColorTheme } from '../uuiui'
import { getAllTargetsAtPoint } from './canvas/dom-lookup'
import { WindowMousePositionRaw } from '../utils/global-positions'

export type ElementContextMenuInstance =
  | 'context-menu-navigator'
  | 'context-menu-canvas'
  | 'context-menu-instance-inspector'

interface ElementContextMenuProps {
  contextMenuInstance: ElementContextMenuInstance
}

const ElementContextMenuItems: Array<ContextMenuItem<CanvasData>> = [
  setAsFocusedElement,
  removeAsFocusedElement,
  lineSeparator,
  scrollToElement,
  cutElements,
  copyElements,
  duplicateElement,
  lineSeparator,
  insert,
  lineSeparator,
  convert,
  lineSeparator,
  wrapInPicker,
  wrapInView,
  unwrap,
  rename,
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

function useCanvasContextMenuItems(
  contextMenuInstance: ElementContextMenuInstance,
  dispatch: EditorDispatch,
): Array<ContextMenuItem<CanvasData>> {
  const elementNamesAndIcons = useNamesAndIconsAllPaths()

  if (contextMenuInstance === 'context-menu-canvas') {
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
          action: () => dispatch([selectComponents([path], false)], 'canvas'),
          isHidden: (data: CanvasData) => {
            const elementsUnderCursor = getAllTargetsAtPoint(
              data.jsxMetadata,
              data.selectedViews,
              data.hiddenInstances,
              'no-filter',
              WindowMousePositionRaw,
              data.scale,
              data.canvasOffset,
            )
            if (elementsUnderCursor != null) {
              return !elementsUnderCursor.some((underCursor: ElementPath) =>
                EP.pathsEqual(underCursor, path),
              )
            } else {
              return true
            }
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
  const isHighlighted = useEditorState(
    (store) => store.editor.highlightedViews.some((view) => EP.pathsEqual(path, view)),
    'SelectableElementItem isHighlighted',
  )
  const highlightElement = React.useCallback(() => dispatch([setHighlightedView(path)]), [
    dispatch,
    path,
  ])

  React.useEffect(() => {
    const current = rawRef.current
    if (current != null) {
      const parent = current.parentElement?.parentElement
      // eslint-disable-next-line no-unused-expressions
      parent?.addEventListener('mousemove', highlightElement)
    }
    return function cleanup() {
      if (current != null) {
        const parent = current.parentElement?.parentElement
        // eslint-disable-next-line no-unused-expressions
        parent?.removeEventListener('mousemove', highlightElement)
      }
    }
  }, [highlightElement])

  return (
    <FlexRow ref={rawRef}>
      <Icn {...iconProps} color={isHighlighted ? 'on-highlight-main' : 'secondary'} />
      <span style={{ paddingLeft: 6 }}>{label}</span>
    </FlexRow>
  )
}

export const ElementContextMenu = React.memo(({ contextMenuInstance }: ElementContextMenuProps) => {
  const { dispatch } = useEditorState((store) => {
    return { dispatch: store.dispatch }
  }, 'ElementContextMenu dispatch')

  const editorSliceRef = useRefEditorState((store) => {
    const resolveFn = store.editor.codeResultCache.curriedResolveFn(store.editor.projectContents)
    return {
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      selectedViews: store.editor.selectedViews,
      jsxMetadata: store.editor.jsxMetadata,
      editorDispatch: store.dispatch,
      projectContents: store.editor.projectContents,
      nodeModules: store.editor.nodeModules.files,
      transientFilesState: store.derived.canvas.transientState.filesState,
      resolve: resolveFn,
      hiddenInstances: store.editor.hiddenInstances,
      scale: store.editor.canvas.scale,
      focusedElementPath: store.editor.focusedElementPath,
    }
  })

  const getData: () => CanvasData = React.useCallback(() => {
    const currentEditor = editorSliceRef.current
    return {
      canvasOffset: currentEditor.canvasOffset,
      selectedViews: currentEditor.selectedViews,
      jsxMetadata: currentEditor.jsxMetadata,
      projectContents: currentEditor.projectContents,
      nodeModules: currentEditor.nodeModules,
      transientFilesState: currentEditor.transientFilesState,
      resolve: currentEditor.resolve,
      hiddenInstances: currentEditor.hiddenInstances,
      scale: currentEditor.scale,
      focusedElementPath: currentEditor.focusedElementPath,
    }
  }, [editorSliceRef])

  const contextMenuItems = useCanvasContextMenuItems(contextMenuInstance, dispatch)

  const portalTarget = document.getElementById(CanvasContextMenuPortalTargetID)
  if (portalTarget == null) {
    return null
  } else {
    return ReactDOM.createPortal(
      <MomentumContextMenu
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
