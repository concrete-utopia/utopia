/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { assertNever } from '../../../core/shared/utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  isJSXConditionalExpression,
} from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { getValueFromComplexMap } from '../../../utils/map'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  defaultElementWarnings,
  DropTargetHint,
  EditorStorePatched,
  isRegularNavigatorEntry,
  navigatorEntriesEqual,
  NavigatorEntry,
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
  navigatorEntry: NavigatorEntry
  getDragSelections: () => Array<DragSelection>
  getSelectedViewsInRange: (index: number) => Array<ElementPath>
  windowStyle: React.CSSProperties
}

const targetElementMetadataSelector = createSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate, target: NavigatorEntry) => target,
  (metadata, target): ElementInstanceMetadata | null => {
    return MetadataUtils.findElementByElementPath(metadata, target.elementPath)
  },
)

const targetInNavigatorItemsSelector = createSelector(
  (store: EditorStorePatched) => store.derived.navigatorTargets,
  (store: EditorStorePatched, target: NavigatorEntry) => target,
  (navigatorTargets, target) => {
    return navigatorTargets.some((navigatorTarget) => {
      return navigatorEntriesEqual(target, navigatorTarget)
    })
  },
)

const canBeReparentedIntoSelector = createSelector(
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
  (_: DerivedSubstate, navigatorEntry: NavigatorEntry) => navigatorEntry,
  (elementWarnings, navigatorEntry) => {
    if (isRegularNavigatorEntry(navigatorEntry)) {
      return (
        getValueFromComplexMap(EP.toString, elementWarnings, navigatorEntry.elementPath) ??
        defaultElementWarnings
      )
    } else {
      return defaultElementWarnings
    }
  },
)

const noOfChildrenSelector = createSelector(
  (store: DerivedSubstate) => store.derived.navigatorTargets,
  (_: DerivedSubstate, targetPath: ElementPath) => targetPath,
  (navigatorTargets, targetPath) => {
    let result = 0
    for (const nt of navigatorTargets) {
      if (EP.isChildOf(nt.elementPath, targetPath)) {
        result += 1
      }
    }
    return result
  },
)

function getNavigatorEntryLabel(
  navigatorEntry: NavigatorEntry,
  labelForTheElement: string,
): string {
  switch (navigatorEntry.type) {
    case 'REGULAR':
      return labelForTheElement
    case 'CONDITIONAL_CLAUSE':
      switch (navigatorEntry.clause) {
        case 'then':
          return 'Then'
        case 'else':
          return 'Else'
        default:
          throw assertNever(navigatorEntry.clause)
      }
    default:
      assertNever(navigatorEntry)
  }
}

export const NavigatorItemWrapper: React.FunctionComponent<
  React.PropsWithChildren<NavigatorItemWrapperProps>
> = React.memo((props) => {
  const isSelected = useEditorState(
    Substores.selectedViews,
    (store) =>
      isRegularNavigatorEntry(props.navigatorEntry) &&
      EP.containsPath(props.navigatorEntry.elementPath, store.editor.selectedViews),
    'NavigatorItemWrapper isSelected',
  )
  const isHighlighted = useEditorState(
    Substores.highlightedHoveredViews,
    (store) =>
      isRegularNavigatorEntry(props.navigatorEntry) &&
      EP.containsPath(props.navigatorEntry.elementPath, store.editor.highlightedViews),
    'NavigatorItemWrapper isHighlighted',
  )

  const noOfChildren = useEditorState(
    Substores.derived,
    (store) => {
      return noOfChildrenSelector(store, props.navigatorEntry.elementPath)
    },
    'NavigatorItemWrapper noOfChildren',
  )

  const canBeReparentedInto = useEditorState(
    Substores.fullStore,
    // this is not good
    (store) => canBeReparentedIntoSelector(store, props.navigatorEntry),
    'NavigatorItemWrapper canBeReparentedIntoSelector',
  )

  const labelForTheElement = useEditorState(
    Substores.metadata,
    (store) => labelSelector(store, props.navigatorEntry),
    'NavigatorItemWrapper labelSelector',
  )
  const label = getNavigatorEntryLabel(props.navigatorEntry, labelForTheElement)

  const elementWarnings = useEditorState(
    Substores.derived,
    (store) => elementWarningsSelector(store, props.navigatorEntry),
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
        if (
          isRegularNavigatorEntry(props.navigatorEntry) &&
          EP.pathsEqual(
            store.editor.navigator.dropTargetHint.displayAtElementPath,
            props.navigatorEntry.elementPath,
          )
        ) {
          possiblyAppropriateDropTargetHint = store.editor.navigator.dropTargetHint
        }
        const elementIsCollapsed = EP.containsPath(
          props.navigatorEntry.elementPath,
          store.editor.navigator.collapsedViews,
        )
        return {
          appropriateDropTargetHint: possiblyAppropriateDropTargetHint,
          renamingTarget: store.editor.navigator.renamingTarget,
          isElementVisible: !EP.containsPath(
            props.navigatorEntry.elementPath,
            store.editor.hiddenInstances,
          ),
          isCollapsed: elementIsCollapsed,
        }
      },
      'NavigatorItemWrapper',
    )

  const navigatorItemProps: NavigatorItemDragAndDropWrapperProps = {
    index: props.index,
    editorDispatch: dispatch,
    navigatorEntry: props.navigatorEntry,
    selected: isSelected,
    highlighted: isHighlighted,
    collapsed: isCollapsed,
    getDragSelections: props.getDragSelections,
    getSelectedViewsInRange: props.getSelectedViewsInRange,
    appropriateDropTargetHint: appropriateDropTargetHint,
    canReparentInto: canBeReparentedInto,
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
