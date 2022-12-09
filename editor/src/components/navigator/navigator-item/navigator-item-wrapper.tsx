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
  JSXElementChild,
  ElementInstanceMetadata,
} from '../../../core/shared/element-template'
import { getValueFromComplexMap } from '../../../utils/map'
import { createSelector } from 'reselect'
import { nullableDeepEquality } from '../../../utils/deep-equality'
import { JSXElementNameKeepDeepEqualityCall } from '../../../utils/deep-equality-instances'
import { useKeepDeepEqualityCall } from '../../../utils/react-performance'
import {
  findUnderlyingComponentImplementationBasedOnMetadata,
  findUnderlyingElementImplementation,
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { getContentsTreeFileFromString } from '../../assets'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { targetPaths } from '../../canvas/canvas-strategies/canvas-strategy-types'
import { isJsxElement } from 'typescript'
import { Either, foldEither, isLeft } from '../../../core/shared/either'

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
  (store: EditorStorePatched) => store.editor.jsxMetadata,
  (store: EditorStorePatched, targetPath: ElementPath) => targetPath,
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

const targetUnderlyingImplementationSelector = createSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  targetElementMetadataSelector,
  (projectContents, elementMetadata): UtopiaJSXComponent | null => {
    if (elementMetadata == null) {
      return null
    }
    return findUnderlyingComponentImplementationBasedOnMetadata(
      projectContents,
      elementMetadata.importInfo,
    )
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
  targetElementMetadataSelector,
  targetUnderlyingImplementationSelector,
  targetInNavigatorItemsSelector,
  (elementMetadata, underlyingComponent, elementInNavigatorTargets) => {
    if (!elementInNavigatorTargets) {
      return false
    }
    return MetadataUtils.targetElementSupportsChildrenForUnderlyingComponent(
      forceNotNull('found null element metadata', elementMetadata),
      underlyingComponent,
    )
  },
)

const elementOriginTypeSelector = createSelector(targetElementMetadataSelector, (elementMetadata) =>
  MetadataUtils.getElementOriginTypeForElement(elementMetadata),
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
    (store: EditorStorePatched) => store.derived.elementWarnings,
    (store: EditorStorePatched) => store.editor.allElementProps,
    (jsxMetadata, elementWarnings, allElementProps) => {
      const labelInner = MetadataUtils.getElementLabel(allElementProps, elementPath, jsxMetadata)

      const elementWarningsInner = getValueFromComplexMap(EP.toString, elementWarnings, elementPath)

      return {
        label: labelInner,

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

  const supportsChildren = useEditorState(
    (store) => targetSupportsChildrenSelector(store, props.elementPath),
    'NavigatorItemWrapper targetSupportsChildrenSelector',
  )

  const elementOriginType = useEditorState(
    (store) => elementOriginTypeSelector(store, props.elementPath),
    'NavigatorItemWrapper elementOriginType',
  )

  const { label, elementWarnings } = useEditorState(selector, 'NavigatorItemWrapper')

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
