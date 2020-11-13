/** @jsx jsx */
import { jsx } from '@emotion/core'
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
  getOpenUIJSFileKey,
  getOpenUtopiaJSXComponentsFromState,
} from '../../editor/store/editor-state'
import {
  UtopiaJSXComponent,
  isUtopiaJSXComponent,
  isIntrinsicElement,
  isJSXElement,
  isJSXAttributeOtherJavaScript,
} from '../../../core/shared/element-template'
import { betterReactMemo } from 'uuiui-deps'
import { getValueFromComplexMap } from '../../../utils/map'
import { createSelector } from 'reselect'
import { UtopiaKeys } from '../../../core/model/utopia-constants'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { eitherToMaybe, isRight } from '../../../core/shared/either'
import { jsxSimpleAttributeToValue } from '../../../core/shared/jsx-attributes'
import { targetRespectsLayout } from '../../../core/layout/layout-helpers'

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
        : MetadataUtils.getElementByInstancePathMaybe(
            store.editor.jsxMetadataKILLME.elements,
            templatePath,
          ),
    (store: EditorStore) =>
      store.derived.canvas.transientState.fileState == null
        ? getOpenImportsFromState(store.editor)
        : store.derived.canvas.transientState.fileState.imports,
    (store: EditorStore) => store.editor.selectedPropsTarget,
    (store: EditorStore) => store.editor.propertyControlsInfo,
    (store: EditorStore) => getOpenUtopiaJSXComponentsFromState(store.editor),
    (store: EditorStore) => getOpenUIJSFileKey(store.editor),
    (
      jsxMetadataKILLME,
      transientState,
      navigatorTargets,
      elementWarnings,
      element,
      imports,
      selectedPropsTarget,
      propertyControlsInfo,
      jsxComponents,
      openFilePath,
    ) => {
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
      const componentInstanceInner = MetadataUtils.isComponentInstance(
        templatePath,
        componentsIncludingScenes,
        jsxMetadataKILLME,
        imports,
      )
      const isSelected = TP.containsPath(templatePath, transientState.selectedViews)
      let properties: { [key: string]: any } = {}
      if (
        componentInstanceInner &&
        staticName != null &&
        element != null &&
        !isIntrinsicElement(staticName) &&
        isFeatureEnabled('Navigator Component Props') &&
        isRight(element.element) &&
        isJSXElement(element.element.value)
      ) {
        const attributes = element.element.value.props
        Object.keys(attributes).forEach((propName) => {
          if (UtopiaKeys.indexOf(propName) === -1) {
            const attribute = attributes[propName]
            if (isJSXAttributeOtherJavaScript(attribute)) {
              properties[propName] = attribute.javascript
            } else {
              properties[propName] = eitherToMaybe(jsxSimpleAttributeToValue(attribute))
            }
          }
        })
        if (element.element.value.children.length > 0 && properties['children'] == null) {
          properties['children'] = element.element.value.children.length + ''
        }
      }
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

      const stylePropsSupported = targetRespectsLayout(
        templatePath,
        propertyControlsInfo,
        imports,
        openFilePath,
        jsxComponents,
        jsxMetadataKILLME,
      )

      return {
        staticElementName: staticName,
        label: labelInner,
        isFlexLayoutedContainer: MetadataUtils.isFlexLayoutedContainer(element),
        yogaDirection: MetadataUtils.getYogaDirection(element),
        yogaWrap: MetadataUtils.getYogaWrap(element),
        componentInstance: componentInstanceInner,
        isAutosizingView: MetadataUtils.isAutoSizingView(element),
        isSelected: isSelected,
        isHighlighted: TP.containsPath(templatePath, transientState.highlightedViews),
        noOfChildren: noOfChildrenInner,
        supportsChildren: supportsChildren,
        imports: imports,
        elementOriginType: elementOriginType,
        elementWarnings: elementWarningsInner ?? defaultElementWarnings,
        properties: properties,
        selectedPropsTarget: selectedPropsTarget,
        stylePropsSupported: stylePropsSupported,
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
      isFlexLayoutedContainer,
      yogaDirection,
      yogaWrap,
      noOfChildren,
      supportsChildren,
      elementOriginType,
      staticElementName,
      label,
      componentInstance,
      isAutosizingView,
      imports,
      elementWarnings,
      properties,
      selectedPropsTarget,
      stylePropsSupported,
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
      staticElementName: staticElementName,
      label: label,
      isFlexLayoutedContainer,
      yogaDirection,
      yogaWrap,
      componentInstance: componentInstance,
      isAutosizingView: isAutosizingView,
      isElementVisible: isElementVisible,
      renamingTarget: renamingTarget,
      imports: imports,
      elementWarnings: elementWarnings,
      properties: properties,
      selectedPropsTarget: selectedPropsTarget,
      stylePropsSupported: stylePropsSupported,
      windowStyle: props.windowStyle,
    }

    return <NavigatorItemContainer {...navigatorItemProps} />
  },
)
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
