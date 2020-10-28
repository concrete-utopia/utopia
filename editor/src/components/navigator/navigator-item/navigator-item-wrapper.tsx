/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import * as TP from '../../../core/shared/template-path'
import { getChildrenOfCollapsedViews } from '../navigator'
import {
  DragSelection,
  NavigatorItemContainer,
  NavigatorItemDragAndDropWrapperProps,
} from './navigator-item-dnd-container'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getOpenImportsFromState, defaultElementWarnings } from '../../editor/store/editor-state'
import { UtopiaJSXComponent, isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { betterReactMemo } from 'uuiui-deps'
import { getValueFromComplexMap } from '../../../utils/map'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  templatePath: TemplatePath
  getDragSelections: () => Array<DragSelection>
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<TemplatePath>
}

export const NavigatorItemWrapper: React.FunctionComponent<NavigatorItemWrapperProps> = betterReactMemo(
  'NavigatorItemWrapper',
  (props) => {
    const {
      dispatch,
      navigatorTargets,
      isSelected,
      isHighlighted,
      collapsedViews,
      dropTargetHint,
      noOfChildren,
      supportsChildren,
      elementOriginType,
      staticElementName,
      label,
      element,
      componentInstance,
      isAutosizingView,
      isElementVisible,
      renamingTarget,
      imports,
      elementWarnings,
    } = useEditorState((store) => {
      const fallbackTransientState = store.derived.canvas.transientState
      const fallbackFileState = fallbackTransientState.fileState
      const elementInner = TP.isScenePath(props.templatePath)
        ? null
        : MetadataUtils.getElementByInstancePathMaybe(
            store.editor.jsxMetadataKILLME,
            props.templatePath,
          )
      const componentsIncludingScenes: Array<UtopiaJSXComponent> =
        fallbackFileState == null
          ? []
          : fallbackFileState.topLevelElementsIncludingScenes.filter(isUtopiaJSXComponent)
      const elementOriginTypeInner = MetadataUtils.getElementOriginType(
        componentsIncludingScenes,
        store.editor.jsxMetadataKILLME,
        props.templatePath,
      )
      const staticName = MetadataUtils.getStaticElementName(
        props.templatePath,
        componentsIncludingScenes,
        store.editor.jsxMetadataKILLME,
      )
      const labelInner = MetadataUtils.getElementLabel(
        props.templatePath,
        store.editor.jsxMetadataKILLME,
        staticName,
      )
      const importsInner =
        fallbackFileState == null
          ? getOpenImportsFromState(store.editor)
          : fallbackFileState.imports
      const componentInstanceInner = MetadataUtils.isComponentInstance(
        props.templatePath,
        componentsIncludingScenes,
        store.editor.jsxMetadataKILLME,
        importsInner,
      )
      const navigatorTargetsInner = store.derived.navigatorTargets
      // FIXME: This is a mitigation for a situation where somehow this component re-renders
      // when the navigatorTargets indicate it shouldn't exist...
      const isInNavigatorTargets = TP.containsPath(props.templatePath, navigatorTargetsInner)
      let noOfChildrenInner: number = 0
      let supportsChildrenInner: boolean = false
      if (isInNavigatorTargets) {
        noOfChildrenInner = MetadataUtils.getImmediateChildren(
          store.editor.jsxMetadataKILLME,
          props.templatePath,
        ).length
        supportsChildrenInner = MetadataUtils.targetSupportsChildren(
          importsInner,
          store.editor.jsxMetadataKILLME,
          props.templatePath,
        )
      }

      const elementWarningsInner = getValueFromComplexMap(
        TP.toString,
        store.derived.elementWarnings,
        props.templatePath,
      )

      return {
        staticElementName: staticName,
        label: labelInner,
        element: elementInner,
        componentInstance: componentInstanceInner,
        isAutosizingView: MetadataUtils.isAutoSizingView(elementInner),
        navigatorTargets: store.derived.navigatorTargets,
        dispatch: store.dispatch,
        selectedViews: store.editor.selectedViews,
        isSelected: TP.containsPath(props.templatePath, fallbackTransientState.selectedViews),
        isHighlighted: TP.containsPath(props.templatePath, fallbackTransientState.highlightedViews),
        collapsedViews: store.editor.navigator.collapsedViews,
        dropTargetHint: store.editor.navigator.dropTargetHint,
        noOfChildren: noOfChildrenInner,
        supportsChildren: supportsChildrenInner,
        imports: importsInner,
        elementOriginType: elementOriginTypeInner,
        renamingTarget: store.editor.navigator.renamingTarget,
        isElementVisible: !TP.containsPath(props.templatePath, store.editor.hiddenInstances),
        elementWarnings: elementWarningsInner ?? defaultElementWarnings,
      }
    }, 'NavigatorItemWrapper')

    const childrenOfCollapsedViews = getChildrenOfCollapsedViews(navigatorTargets, collapsedViews)
    const isCollapsed = TP.containsPath(props.templatePath, collapsedViews)
    const isAncestorCollapsed = TP.containsPath(props.templatePath, childrenOfCollapsedViews)

    const navigatorItemProps: NavigatorItemDragAndDropWrapperProps = {
      index: props.index,
      editorDispatch: dispatch,
      templatePath: props.templatePath,
      selected: isSelected,
      highlighted: isHighlighted,
      collapsed: isCollapsed,
      ancestorCollapsed: isAncestorCollapsed,
      getDragSelections: props.getDragSelections,
      getMaximumDistance: props.getMaximumDistance,
      getSelectedViewsInRange: props.getSelectedViewsInRange,
      dropTargetHint: dropTargetHint,
      supportsChildren: supportsChildren,
      noOfChildren: noOfChildren,
      elementOriginType: elementOriginType,
      staticElementName: staticElementName,
      label: label,
      element: element,
      componentInstance: componentInstance,
      isAutosizingView: isAutosizingView,
      isElementVisible: isElementVisible,
      renamingTarget: renamingTarget,
      imports: imports,
      elementWarnings: elementWarnings,
    }

    return <NavigatorItemContainer {...navigatorItemProps} />
  },
)
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
