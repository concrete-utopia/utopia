/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  isParseSuccess,
  isTextFile,
  ParseSuccess,
  ElementPath,
} from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import {
  DragSelection,
  NavigatorItemContainer,
  NavigatorItemDragAndDropWrapperProps,
} from './navigator-item-dnd-container'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  defaultElementWarnings,
  DropTargetHint,
  EditorStorePatched,
  TransientFileState,
} from '../../editor/store/editor-state'
import { UtopiaJSXComponent, isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { getValueFromComplexMap } from '../../../utils/map'
import { createSelector } from 'reselect'
import { nullableDeepEquality } from '../../../utils/deep-equality'
import { JSXElementNameKeepDeepEqualityCall } from '../../../utils/deep-equality-instances'
import { useKeepDeepEqualityCall } from '../../../utils/react-performance'
import {
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { getContentsTreeFileFromString } from '../../assets'
import { emptyImports } from '../../../core/workers/common/project-file-utils'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  elementPath: ElementPath
  getDragSelections: () => Array<DragSelection>
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<ElementPath>
  windowStyle: React.CSSProperties
}

const navigatorItemWrapperSelector = createSelector(
  (store: EditorStorePatched) => store.editor.jsxMetadata,
  (store: EditorStorePatched) => store.editor.selectedViews,
  (store: EditorStorePatched) => store.editor.highlightedViews,
  (store: EditorStorePatched) => store.derived.transientState,
  (store: EditorStorePatched) => store.derived.navigatorTargets,
  (store: EditorStorePatched) => store.derived.elementWarnings,
  (store: EditorStorePatched) => store.editor.projectContents,
  (store: EditorStorePatched) => store.editor.nodeModules.files,
  (store: EditorStorePatched) => store.editor.canvas.openFile?.filename ?? null,
  (store: EditorStorePatched) => store.editor.allElementProps,
  (_: EditorStorePatched, elementPath: ElementPath) => elementPath,
  (
    jsxMetadata,
    selectedViews,
    highlightedViews,
    transientState,
    navigatorTargets,
    elementWarnings,
    projectContents,
    nodeModules,
    currentFilePath,
    allElementProps,
    elementPath,
  ) => {
    const underlying = normalisePathToUnderlyingTarget(
      projectContents,
      nodeModules,
      forceNotNull('Should be a file path.', currentFilePath),
      elementPath,
    )
    const elementFilePath =
      underlying.type === 'NORMALISE_PATH_SUCCESS' ? underlying.filePath : currentFilePath
    const elementProjectFile =
      elementFilePath == null
        ? null
        : getContentsTreeFileFromString(projectContents, elementFilePath)
    const elementTextFile = isTextFile(elementProjectFile) ? elementProjectFile : null
    let parsedElementFile: ParseSuccess | null = null
    if (elementTextFile != null && isParseSuccess(elementTextFile.fileContents.parsed)) {
      parsedElementFile = elementTextFile.fileContents.parsed
    }
    const fileState =
      elementFilePath == null ? null : transientState.filesState?.[elementFilePath] ?? null
    const topLevelElements =
      fileState?.topLevelElementsIncludingScenes ?? parsedElementFile?.topLevelElements ?? []
    const componentsIncludingScenes = topLevelElements.filter(isUtopiaJSXComponent)

    const elementOriginType = MetadataUtils.getElementOriginType(
      componentsIncludingScenes,
      elementPath,
    )
    const staticName = MetadataUtils.getStaticElementName(elementPath, componentsIncludingScenes)
    const labelInner = MetadataUtils.getElementLabel(
      allElementProps,
      elementPath,
      jsxMetadata,
      staticName,
    )
    // FIXME: This is a mitigation for a situation where somehow this component re-renders
    // when the navigatorTargets indicate it shouldn't exist...
    const isInNavigatorTargets = EP.containsPath(elementPath, navigatorTargets)
    let noOfChildrenInner: number = 0
    let supportsChildren: boolean = false
    if (isInNavigatorTargets) {
      noOfChildrenInner = MetadataUtils.getImmediateChildren(jsxMetadata, elementPath).length
      supportsChildren = MetadataUtils.targetSupportsChildren(
        projectContents,
        currentFilePath,
        jsxMetadata,
        elementPath,
      )
    }

    const elementWarningsInner = getValueFromComplexMap(EP.toString, elementWarnings, elementPath)

    return {
      staticElementName: staticName,
      label: labelInner,
      isSelected: EP.containsPath(elementPath, transientState.selectedViews ?? selectedViews),
      isHighlighted: EP.containsPath(
        elementPath,
        transientState.highlightedViews ?? highlightedViews,
      ),
      noOfChildren: noOfChildrenInner,
      supportsChildren: supportsChildren,
      elementOriginType: elementOriginType,
      elementWarnings: elementWarningsInner ?? defaultElementWarnings,
    }
  },
)

const nullableJSXElementNameKeepDeepEquality = nullableDeepEquality(
  JSXElementNameKeepDeepEqualityCall,
)

export const NavigatorItemWrapper: React.FunctionComponent<
  React.PropsWithChildren<NavigatorItemWrapperProps>
> = React.memo((props) => {
  const {
    isSelected,
    isHighlighted,
    noOfChildren,
    supportsChildren,
    elementOriginType,
    staticElementName,
    label,
    elementWarnings,
  } = useEditorState(
    (store) => navigatorItemWrapperSelector(store, props.elementPath),
    'NavigatorItemWrapper',
  )

  const { isElementVisible, renamingTarget, appropriateDropTargetHint, dispatch, isCollapsed } =
    useEditorState((store) => {
      // Only capture this if it relates to the current navigator item, as it may change while
      // dragging around the navigator but we don't want the entire navigator to re-render each time.
      let possiblyAppropriateDropTargetHint: DropTargetHint | null = null
      if (EP.pathsEqual(store.editor.navigator.dropTargetHint.target, props.elementPath)) {
        possiblyAppropriateDropTargetHint = store.editor.navigator.dropTargetHint
      }
      const elementIsCollapsed = EP.containsPath(
        props.elementPath,
        store.editor.navigator.collapsedViews,
      )
      return {
        dispatch: store.dispatch,
        appropriateDropTargetHint: possiblyAppropriateDropTargetHint,
        renamingTarget: store.editor.navigator.renamingTarget,
        isElementVisible: !EP.containsPath(props.elementPath, store.editor.hiddenInstances),
        isCollapsed: elementIsCollapsed,
      }
    }, 'NavigatorItemWrapper')

  const deepReferenceStaticElementName = useKeepDeepEqualityCall(
    staticElementName,
    nullableJSXElementNameKeepDeepEquality,
  )

  const navigatorItemProps: NavigatorItemDragAndDropWrapperProps = {
    index: props.index,
    editorDispatch: dispatch,
    elementPath: props.elementPath,
    selected: isSelected,
    highlighted: isHighlighted,
    collapsed: isCollapsed,
    getDragSelections: props.getDragSelections,
    getMaximumDistance: props.getMaximumDistance,
    getSelectedViewsInRange: props.getSelectedViewsInRange,
    appropriateDropTargetHint: appropriateDropTargetHint,
    supportsChildren: supportsChildren,
    noOfChildren: noOfChildren,
    staticElementName: deepReferenceStaticElementName,
    label: label,
    isElementVisible: isElementVisible,
    renamingTarget: renamingTarget,
    elementWarnings: elementWarnings,
    windowStyle: props.windowStyle,
  }

  return <NavigatorItemContainer {...navigatorItemProps} />
})
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
