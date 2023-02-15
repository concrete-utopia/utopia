/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Either, foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXElementChild,
  jsxElementName,
} from '../../../core/shared/element-template'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { nullableDeepEquality } from '../../../utils/deep-equality'
import { JSXElementNameKeepDeepEqualityCall } from '../../../utils/deep-equality-instances'
import { getValueFromComplexMap } from '../../../utils/map'
import { useKeepDeepEqualityCall } from '../../../utils/react-performance'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  defaultElementWarnings,
  DropTargetHint,
  EditorStorePatched,
} from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { DerivedSubstate, MetadataSubstate } from '../../editor/store/store-hook-substore-types'
import {
  DragSelection,
  NavigatorItemContainer,
  NavigatorItemDragAndDropWrapperProps,
} from './navigator-item-dnd-container'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  elementPath: ElementPath
  getDragSelections: () => Array<DragSelection>
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<ElementPath>
  windowStyle: React.CSSProperties
}

const targetElementMetadataSelector = createSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate, targetPath: ElementPath) => targetPath,
  (metadata, targetPath): ElementInstanceMetadata | null => {
    return MetadataUtils.findElementByElementPath(metadata, targetPath)
  },
)

const targetJsxElementSelector = createSelector(
  targetElementMetadataSelector,
  (metadata): Either<string, JSXElementChild> | undefined => {
    return metadata?.element
  },
)

const targetInNavigatorItemsSelector = createSelector(
  (store: EditorStorePatched) => store.derived.navigatorTargets,
  (store: EditorStorePatched, targetPath: ElementPath) => targetPath,
  (navigatorTargets, targetPath) => {
    return EP.containsPath(targetPath, navigatorTargets)
  },
)

const targetSupportsChildrenSelector = createSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  targetElementMetadataSelector,
  targetInNavigatorItemsSelector,
  (projectContents, elementMetadata, elementInNavigatorTargets) => {
    if (!elementInNavigatorTargets || elementMetadata == null) {
      return false
    }
    return MetadataUtils.targetElementSupportsChildren(projectContents, elementMetadata)
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

const labelSelector = createSelector(
  targetElementMetadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (elementMetadata, allElementProps) => {
    if (elementMetadata == null) {
      return 'Element 👻'
    }
    return MetadataUtils.getElementLabelFromMetadata(allElementProps, elementMetadata)
  },
)

const elementWarningsSelector = createSelector(
  (store: DerivedSubstate) => store.derived.elementWarnings,
  (_: DerivedSubstate, elementPath: ElementPath) => elementPath,
  (elementWarnings, elementPath) => {
    return (
      getValueFromComplexMap(EP.toString, elementWarnings, elementPath) ?? defaultElementWarnings
    )
  },
)

const noOfChildrenSelector = createSelector(
  (store: DerivedSubstate) => store.derived.navigatorTargets,
  (_: DerivedSubstate, targetPath: ElementPath) => targetPath,
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
  const isSelected = useEditorState(
    Substores.selectedViews,
    (store) => EP.containsPath(props.elementPath, store.editor.selectedViews),
    'NavigatorItemWrapper isSelected',
  )
  const isHighlighted = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => EP.containsPath(props.elementPath, store.editor.highlightedViews),
    'NavigatorItemWrapper isHighlighted',
  )

  const noOfChildren = useEditorState(
    Substores.derived,
    (store) => {
      return noOfChildrenSelector(store, props.elementPath)
    },
    'NavigatorItemWrapper noOfChildren',
  )

  const supportsChildren = useEditorState(
    Substores.fullStore,
    // this is not good
    (store) => targetSupportsChildrenSelector(store, props.elementPath),
    'NavigatorItemWrapper targetSupportsChildrenSelector',
  )

  const label = useEditorState(
    Substores.metadata,
    (store) => labelSelector(store, props.elementPath),
    'NavigatorItemWrapper labelSelector',
  )

  const elementWarnings = useEditorState(
    Substores.derived,
    (store) => elementWarningsSelector(store, props.elementPath),
    'NavigatorItemWrapper elementWarningsSelector',
  )

  const visibleNavigatorTargets = useEditorState(
    Substores.derived,
    (store) => store.derived.visibleNavigatorTargets,
    'NavigatorItemWrapper navigatorTargets',
  )
  const dispatch = useDispatch()
  const { isElementVisible, renamingTarget, appropriateDropTargetHint, isCollapsed } =
    useEditorState(
      Substores.restOfEditor,
      (store) => {
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
          appropriateDropTargetHint: possiblyAppropriateDropTargetHint,
          renamingTarget: store.editor.navigator.renamingTarget,
          isElementVisible: !EP.containsPath(props.elementPath, store.editor.hiddenInstances),
          isCollapsed: elementIsCollapsed,
        }
      },
      'NavigatorItemWrapper',
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
    label: label,
    isElementVisible: isElementVisible,
    renamingTarget: renamingTarget,
    elementWarnings: elementWarnings,
    windowStyle: props.windowStyle,
    visibleNavigatorTargets: visibleNavigatorTargets,
  }

  return <NavigatorItemContainer {...navigatorItemProps} />
})
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
