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
import {
  UtopiaJSXComponent,
  isUtopiaJSXComponent,
  isJSXElement,
  jsxElementName,
} from '../../../core/shared/element-template'
import { getValueFromComplexMap } from '../../../utils/map'
import { createSelector } from 'reselect'
import { nullableDeepEquality } from '../../../utils/deep-equality'
import { JSXElementNameKeepDeepEqualityCall } from '../../../utils/deep-equality-instances'
import { useKeepDeepEqualityCall } from '../../../utils/react-performance'
import {
  findUnderlyingElementImplementation,
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { getContentsTreeFileFromString } from '../../assets'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { targetPaths } from '../../canvas/canvas-strategies/canvas-strategy-types'
import { isJsxElement } from 'typescript'
import { foldEither, isLeft } from '../../../core/shared/either'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  elementPath: ElementPath
  getDragSelections: () => Array<DragSelection>
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<ElementPath>
  windowStyle: React.CSSProperties
}

const targetJsxElementSelector = createSelector(
  (store: EditorStorePatched) => store.editor.jsxMetadata,
  (store: EditorStorePatched, targetPath: ElementPath) => targetPath,
  (metadata, targetPath) => {
    return MetadataUtils.findElementByElementPath(metadata, targetPath)?.element
  },
)

const staticNameSelector = createSelector(targetJsxElementSelector, (targetElement) => {
  if (targetElement == null) {
    return null
  }
  return foldEither(
    (intrinsic) => jsxElementName(intrinsic, []),
    (element) => (isJSXElement(element) ? element.name : null),
    targetElement,
  )
})

const navigatorItemWrapperSelectorFactory = (elementPath: ElementPath) =>
  createSelector(
    (store: EditorStorePatched) => store.editor.jsxMetadata,
    (store: EditorStorePatched) => store.derived.transientState,
    (store: EditorStorePatched) => store.derived.navigatorTargets,
    (store: EditorStorePatched) => store.derived.elementWarnings,
    (store: EditorStorePatched) => store.editor.projectContents,
    (store: EditorStorePatched) => store.editor.nodeModules.files,
    (store: EditorStorePatched) => store.editor.canvas.openFile?.filename ?? null,
    (store: EditorStorePatched) => store.editor.allElementProps,
    (
      jsxMetadata,
      transientState,
      navigatorTargets,
      elementWarnings,
      projectContents,
      nodeModules,
      currentFilePath,
      allElementProps,
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

      const labelInner = MetadataUtils.getElementLabel(allElementProps, elementPath, jsxMetadata)
      // FIXME: This is a mitigation for a situation where somehow this component re-renders
      // when the navigatorTargets indicate it shouldn't exist...
      const isInNavigatorTargets = EP.containsPath(elementPath, navigatorTargets)
      let supportsChildren: boolean = false
      if (isInNavigatorTargets) {
        supportsChildren = MetadataUtils.targetSupportsChildren(
          projectContents,
          currentFilePath,
          jsxMetadata,
          elementPath,
        )
      }

      const elementWarningsInner = getValueFromComplexMap(EP.toString, elementWarnings, elementPath)

      return {
        label: labelInner,

        supportsChildren: supportsChildren,
        elementOriginType: elementOriginType,
        elementWarnings: elementWarningsInner ?? defaultElementWarnings,
      }
    },
  )

const noOfChildrenSelector = createSelector(
  (store: EditorStorePatched) => store.derived.navigatorTargets,
  (_: EditorStorePatched, targetPath: ElementPath) => targetPath,
  (navigatorTargets, targetPath) => {
    let result = 0
    for (const nt of navigatorTargets) {
      if (EP.isChildOf(nt, targetPath)) {
        result += 1
      }
    }
    return result
  },
)

const nullableJSXElementNameKeepDeepEquality = nullableDeepEquality(
  JSXElementNameKeepDeepEqualityCall,
)

export const NavigatorItemWrapper: React.FunctionComponent<
  React.PropsWithChildren<NavigatorItemWrapperProps>
> = React.memo((props) => {
  const selector = React.useMemo(
    () => navigatorItemWrapperSelectorFactory(props.elementPath),
    [props.elementPath],
  )

  const { isSelected, isHighlighted } = useEditorState(
    (store) => ({
      isSelected: EP.containsPath(props.elementPath, store.editor.selectedViews),
      isHighlighted: EP.containsPath(props.elementPath, store.editor.highlightedViews),
    }),
    'NavigatorItemWrapper isSelected',
  )

  const noOfChildren = useEditorState((store) => {
    return noOfChildrenSelector(store, props.elementPath)
  }, 'NavigatorItemWrapper noOfChildren')

  const staticElementName = useEditorState(
    (store) => staticNameSelector(store, props.elementPath),
    'NavigatorItemWrapper staticName',
  )

  const { supportsChildren, elementOriginType, label, elementWarnings } = useEditorState(
    selector,
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
    elementOriginType: elementOriginType,
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
