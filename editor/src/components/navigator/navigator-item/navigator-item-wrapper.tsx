/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { TemplatePath } from '../../../core/shared/project-file-types'
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
} from '../../editor/store/editor-state'
import { UtopiaJSXComponent, isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { getValueFromComplexMap } from '../../../utils/map'
import { createSelector } from 'reselect'
import { nullableDeepEquality } from '../../../utils/deep-equality'
import { JSXElementNameKeepDeepEqualityCall } from '../../../utils/deep-equality-instances'
import { betterReactMemo, useKeepDeepEqualityCall } from '../../../utils/react-performance'

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
    (store: EditorStore) => store.editor.jsxMetadataKILLME,
    (store: EditorStore) => store.derived.canvas.transientState,
    (store: EditorStore) => store.derived.navigatorTargets,
    (store: EditorStore) => store.derived.elementWarnings,
    (store: EditorStore) =>
      TP.isScenePath(templatePath)
        ? null
        : MetadataUtils.getElementByInstancePathMaybe(store.editor.jsxMetadataKILLME, templatePath),
    (store: EditorStore) =>
      store.derived.canvas.transientState.fileState == null
        ? getOpenImportsFromState(store.editor)
        : store.derived.canvas.transientState.fileState.imports,
    (jsxMetadataKILLME, transientState, navigatorTargets, elementWarnings, element, imports) => {
      const fileState = transientState.fileState

      const componentsIncludingScenes: Array<UtopiaJSXComponent> =
        fileState == null
          ? []
          : fileState.topLevelElementsIncludingScenes.filter(isUtopiaJSXComponent)
      const elementOriginType = MetadataUtils.getElementOriginType(
        componentsIncludingScenes,
        templatePath,
      )
      const staticName = MetadataUtils.getStaticElementName(templatePath, componentsIncludingScenes)
      const labelInner = MetadataUtils.getElementLabel(templatePath, jsxMetadataKILLME, staticName)
      // FIXME: This is a mitigation for a situation where somehow this component re-renders
      // when the navigatorTargets indicate it shouldn't exist...
      const isInNavigatorTargets = TP.containsPath(templatePath, navigatorTargets)
      let noOfChildrenInner: number = 0
      let supportsChildren: boolean = false
      if (isInNavigatorTargets) {
        noOfChildrenInner = MetadataUtils.getImmediateChildren(jsxMetadataKILLME, templatePath)
          .length
        supportsChildren = MetadataUtils.targetSupportsChildren(
          imports,
          jsxMetadataKILLME,
          templatePath,
        )
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
