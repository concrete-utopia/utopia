/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import {
  isParseSuccess,
  isTextFile,
  ParseSuccess,
  TemplatePath,
} from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import * as TP from '../../../core/shared/template-path'
import {
  DragSelection,
  NavigatorItemContainer,
  NavigatorItemDragAndDropWrapperProps,
} from './navigator-item-dnd-container'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getOpenImportsFromState,
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
  templatePath: TemplatePath
  getDragSelections: () => Array<DragSelection>
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<TemplatePath>
  windowStyle: React.CSSProperties
}

const navigatorItemWrapperSelectorFactory = (templatePath: TemplatePath) =>
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
        TP.instancePathForElementAtPathDontThrowOnScene(templatePath),
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
      const imports = fileState?.imports ?? parsedElementFile?.imports ?? emptyImports()
      const topLevelElements =
        fileState?.topLevelElementsIncludingScenes ?? parsedElementFile?.topLevelElements ?? []
      const componentsIncludingScenes = topLevelElements.filter(isUtopiaJSXComponent)

      const elementOriginType = MetadataUtils.getElementOriginType(
        componentsIncludingScenes,
        templatePath,
      )
      const staticName = MetadataUtils.getStaticElementName(templatePath, componentsIncludingScenes)
      const labelInner = MetadataUtils.getElementLabel(templatePath, jsxMetadata, staticName)
      // FIXME: This is a mitigation for a situation where somehow this component re-renders
      // when the navigatorTargets indicate it shouldn't exist...
      const isInNavigatorTargets = TP.containsPath(templatePath, navigatorTargets)
      let noOfChildrenInner: number = 0
      let supportsChildren: boolean = false
      if (isInNavigatorTargets) {
        noOfChildrenInner = MetadataUtils.getImmediateChildren(jsxMetadata, templatePath).length
        supportsChildren = MetadataUtils.targetSupportsChildren(imports, jsxMetadata, templatePath)
      }

      const elementWarningsInner = getValueFromComplexMap(
        TP.toString,
        elementWarnings,
        templatePath,
      )

      return {
        staticElementName: staticName,
        label: labelInner,
        isSelected: TP.containsPath(templatePath, transientState.selectedViews),
        isHighlighted: TP.containsPath(templatePath, transientState.highlightedViews),
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
    const selector = React.useMemo(() => navigatorItemWrapperSelectorFactory(props.templatePath), [
      props.templatePath,
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
        isElementVisible: !TP.containsPath(props.templatePath, store.editor.hiddenInstances),
      }),
      'NavigatorItemWrapper',
    )

    const isCollapsed = TP.containsPath(props.templatePath, collapsedViews)

    const deepReferenceStaticElementName = useKeepDeepEqualityCall(
      staticElementName,
      nullableDeepEquality(JSXElementNameKeepDeepEqualityCall()),
    )

    const navigatorItemProps: NavigatorItemDragAndDropWrapperProps = {
      index: props.index,
      editorDispatch: dispatch,
      templatePath: props.templatePath,
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
