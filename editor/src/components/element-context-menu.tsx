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
  setAsFocusedElement,
  scrollToElement,
} from './context-menu-items'
import { MomentumContextMenu } from './context-menu-wrapper'
import { useRefEditorState, useEditorState } from './editor/store/store-hook'
import { filterScenes } from '../core/shared/template-path'
import { betterReactMemo } from '../uuiui-deps'
import { CanvasContextMenuPortalTargetID } from '../core/shared/utils'
import { EditorDispatch } from './editor/action-types'
import { selectComponents, setHighlightedView } from './editor/actions/action-creators'
import * as TP from '../core/shared/template-path'
import { TemplatePath } from '../core/shared/project-file-types'
import { useNamesAndIconsAllPaths } from './inspector/common/name-and-icon-hook'
import { FlexRow, Icn, IcnProps } from '../uuiui'
import { getOpenUIJSFileKey } from './editor/store/editor-state'
import { WindowMousePositionRaw } from '../templates/editor-canvas'
import { getAllTargetsAtPoint } from './canvas/dom-lookup'

export type ElementContextMenuInstance =
  | 'context-menu-navigator'
  | 'context-menu-canvas'
  | 'context-menu-instance-inspector'

interface ElementContextMenuProps {
  contextMenuInstance: ElementContextMenuInstance
}

const ElementContextMenuItems: Array<ContextMenuItem<CanvasData>> = [
  setAsFocusedElement,
  lineSeparator,
  scrollToElement,
  cutElements,
  copyElements,
  duplicateElement,
  lineSeparator,
  group,
  ungroup,
  wrapInView,
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
              data.focusedElementPath,
              'no-filter',
              WindowMousePositionRaw,
              data.scale,
              data.canvasOffset,
            )
            if (elementsUnderCursor != null) {
              return !elementsUnderCursor.some((underCursor: TemplatePath) =>
                TP.pathsEqual(underCursor, path),
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
  path: TemplatePath
  iconProps: IcnProps
  label: string
}

const SelectableElementItem = (props: SelectableElementItemProps) => {
  const rawRef = React.useRef<HTMLDivElement>(null)
  const { dispatch, path, iconProps, label } = props
  const isHighlighted = useEditorState(
    (store) => store.editor.highlightedViews.some((view) => TP.pathsEqual(path, view)),
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
      <Icn {...iconProps} color={isHighlighted ? 'white' : 'darkgray'} />
      <span style={{ paddingLeft: 6 }}>{label}</span>
    </FlexRow>
  )
}

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
        jsxMetadata: store.editor.jsxMetadata,
        editorDispatch: store.dispatch,
        currentFilePath: getOpenUIJSFileKey(store.editor),
        projectContents: store.editor.projectContents,
        nodeModules: store.editor.nodeModules.files,
        transientFilesState: store.derived.canvas.transientState.filesState,
        resolve: store.editor.codeResultCache.resolve,
        focusedElementPath: store.editor.focusedElementPath,
        hiddenInstances: store.editor.hiddenInstances,
        scale: store.editor.canvas.scale,
      }
    })

    const getData: () => CanvasData = React.useCallback(() => {
      const currentEditor = editorSliceRef.current
      return {
        canvasOffset: currentEditor.canvasOffset,
        selectedViews: filterScenes(currentEditor.selectedViews),
        jsxMetadata: currentEditor.jsxMetadata,
        currentFilePath: currentEditor.currentFilePath,
        projectContents: currentEditor.projectContents,
        nodeModules: currentEditor.nodeModules,
        transientFilesState: currentEditor.transientFilesState,
        resolve: currentEditor.resolve,
        focusedElementPath: currentEditor.focusedElementPath,
        hiddenInstances: currentEditor.hiddenInstances,
        scale: currentEditor.scale,
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
  },
)
