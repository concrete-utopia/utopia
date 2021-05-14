/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
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
  EditorStore,
  TransientFileState,
} from '../../editor/store/editor-state'
import { UtopiaJSXComponent, isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { getValueFromComplexMap } from '../../../utils/map'
import { createSelector } from 'reselect'
import { nullableDeepEquality } from '../../../utils/deep-equality'
import { JSXElementNameKeepDeepEqualityCall } from '../../../utils/deep-equality-instances'
import { betterReactMemo, useKeepDeepEqualityCall } from '../../../utils/react-performance'
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

const navigatorItemWrapperSelectorFactory = (elementPath: ElementPath) =>
  createSelector(
    (store: EditorStore) => store.editor.jsxMetadata,
    (store: EditorStore) => store.derived.canvas.transientState,
    (store: EditorStore) => store.derived.navigatorTargets,
    (store: EditorStore) => store.derived.elementWarnings,
    (store: EditorStore) => store.editor.projectContents,
    (store: EditorStore) => store.editor.nodeModules.files,
    (store: EditorStore) => store.editor.canvas.openFile?.filename ?? null,
    (
      jsxMetadata,
      transientState,
      navigatorTargets,
      elementWarnings,
      projectContents,
      nodeModules,
      currentFilePath,
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
      const labelInner = MetadataUtils.getElementLabel(elementPath, jsxMetadata, staticName)
      // FIXME: This is a mitigation for a situation where somehow this component re-renders
      // when the navigatorTargets indicate it shouldn't exist...
      const isInNavigatorTargets = EP.containsPath(elementPath, navigatorTargets)
      let noOfChildrenInner: number = 0
      let supportsChildren: boolean = false
      if (isInNavigatorTargets) {
        noOfChildrenInner = MetadataUtils.getImmediateChildren(jsxMetadata, elementPath).length
        supportsChildren = MetadataUtils.targetSupportsChildren(jsxMetadata, elementPath)
      }

      const elementWarningsInner = getValueFromComplexMap(EP.toString, elementWarnings, elementPath)

      return {
        staticElementName: staticName,
        label: labelInner,
        isSelected: EP.containsPath(elementPath, transientState.selectedViews),
        isHighlighted: EP.containsPath(elementPath, transientState.highlightedViews),
        noOfChildren: noOfChildrenInner,
        supportsChildren: supportsChildren,
        elementOriginType: elementOriginType,
        elementWarnings: elementWarningsInner ?? defaultElementWarnings,
      }
    },
  )

export const NavigatorItemWrapper: React.FunctionComponent<NavigatorItemWrapperProps> = betterReactMemo(
  'NavigatorItemWrapper',
  (props) => {
    const selector = React.useMemo(() => navigatorItemWrapperSelectorFactory(props.elementPath), [
      props.elementPath,
    ])
    const {
      isSelected,
      isHighlighted,
      noOfChildren,
      supportsChildren,
      elementOriginType,
      staticElementName,
      label,
      elementWarnings,
    } = useEditorState(selector, 'NavigatorItemWrapper')

    const {
      isElementVisible,
      renamingTarget,
      collapsedViews,
      dropTargetHint,
      dispatch,
    } = useEditorState(
      (store) => ({
        dispatch: store.dispatch,
        selectedViews: store.editor.selectedViews,
        collapsedViews: store.editor.navigator.collapsedViews,
        dropTargetHint: store.editor.navigator.dropTargetHint,
        renamingTarget: store.editor.navigator.renamingTarget,
        isElementVisible: !EP.containsPath(props.elementPath, store.editor.hiddenInstances),
      }),
      'NavigatorItemWrapper',
    )

    const isCollapsed = EP.containsPath(props.elementPath, collapsedViews)

    const deepReferenceStaticElementName = useKeepDeepEqualityCall(
      staticElementName,
      nullableDeepEquality(JSXElementNameKeepDeepEqualityCall()),
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
      dropTargetHint: dropTargetHint,
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
  },
)
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
