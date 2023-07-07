/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { assertNever } from '../../../core/shared/utils'
import { createCachedSelector } from 're-reselect'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  JSXConditionalExpression,
} from '../../../core/shared/element-template'
import {
  getJSXElementNameLastPart,
  isNullJSXAttributeValue,
} from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import type {
  DropTargetHint,
  EditorStorePatched,
  NavigatorEntry,
} from '../../editor/store/editor-state'
import {
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  isSyntheticNavigatorEntry,
  navigatorEntriesEqual,
  navigatorEntryToKey,
} from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type {
  DerivedSubstate,
  MetadataSubstate,
} from '../../editor/store/store-hook-substore-types'
import type {
  ConditionalClauseNavigatorItemContainerProps,
  NavigatorItemDragAndDropWrapperProps,
  NavigatorItemDragAndDropWrapperPropsBase,
  SyntheticNavigatorItemContainerProps,
} from './navigator-item-dnd-container'
import {
  ConditionalClauseNavigatorItemContainer,
  NavigatorItemContainer,
  SyntheticNavigatorItemContainer,
} from './navigator-item-dnd-container'
import { navigatorDepth } from '../navigator-utils'
import { maybeConditionalExpression } from '../../../core/model/conditionals'
import { front } from '../../../utils/utils'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  navigatorEntry: NavigatorEntry
  getCurrentlySelectedEntries: () => Array<NavigatorEntry>
  getSelectedViewsInRange: (index: number) => Array<ElementPath>
  windowStyle: React.CSSProperties
}

const targetElementMetadataSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate, target: NavigatorEntry) => target,
  (metadata, target): ElementInstanceMetadata | null => {
    return MetadataUtils.findElementByElementPath(metadata, target.elementPath)
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

const targetInNavigatorItemsSelector = createCachedSelector(
  (store: EditorStorePatched) => store.derived.navigatorTargets,
  (store: EditorStorePatched, target: NavigatorEntry) => target,
  (navigatorTargets, target) => {
    return navigatorTargets.some((navigatorTarget) => {
      return navigatorEntriesEqual(target, navigatorTarget)
    })
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

const elementSupportsChildrenSelector = createCachedSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  targetElementMetadataSelector,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  targetInNavigatorItemsSelector,
  (projectContents, metadata, elementMetadata, pathTrees, elementInNavigatorTargets) => {
    if (!elementInNavigatorTargets || elementMetadata == null) {
      return false
    }
    return MetadataUtils.targetElementSupportsChildren(
      projectContents,
      elementMetadata.elementPath,
      metadata,
      pathTrees,
    )
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

export const labelSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  targetElementMetadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (metadata, elementMetadata, allElementProps, pathTrees) => {
    if (elementMetadata == null) {
      return 'Element 👻'
    }
    return MetadataUtils.getElementLabelFromMetadata(
      metadata,
      allElementProps,
      pathTrees,
      elementMetadata,
    )
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

const noOfChildrenSelector = createCachedSelector(
  (store: DerivedSubstate) => store.derived.navigatorTargets,
  (_: DerivedSubstate, navigatorEntry: NavigatorEntry) => navigatorEntry,
  (navigatorTargets, navigatorEntry) => {
    let result = 0
    for (const nt of navigatorTargets) {
      if (
        isRegularNavigatorEntry(navigatorEntry) &&
        EP.isChildOf(nt.elementPath, navigatorEntry.elementPath)
      ) {
        result += 1
      }
    }
    return result
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

export function getNavigatorEntryLabel(
  navigatorEntry: NavigatorEntry,
  labelForTheElement: string,
): string {
  switch (navigatorEntry.type) {
    case 'REGULAR':
      return labelForTheElement
    case 'CONDITIONAL_CLAUSE':
      switch (navigatorEntry.clause) {
        case 'true-case':
          return 'TRUE'
        case 'false-case':
          return 'FALSE'
        default:
          throw assertNever(navigatorEntry.clause)
      }
    case 'SYNTHETIC': {
      switch (navigatorEntry.childOrAttribute.type) {
        case 'JSX_ELEMENT':
          return getJSXElementNameLastPart(navigatorEntry.childOrAttribute.name)
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
          return '(code)'
        case 'JSX_TEXT_BLOCK':
          return navigatorEntry.childOrAttribute.text
        case 'JSX_FRAGMENT':
          return 'Fragment'
        case 'JSX_CONDITIONAL_EXPRESSION':
          return 'Conditional'
        case 'ATTRIBUTE_VALUE':
          return `${navigatorEntry.childOrAttribute.value}`
        case 'ATTRIBUTE_NESTED_ARRAY':
          return '(code)'
        case 'ATTRIBUTE_NESTED_OBJECT':
          return '(code)'
        case 'ATTRIBUTE_FUNCTION_CALL':
          return '(code)'
        default:
          throw assertNever(navigatorEntry.childOrAttribute)
      }
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
      !isConditionalClauseNavigatorEntry(props.navigatorEntry) &&
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
      return noOfChildrenSelector(store, props.navigatorEntry)
    },
    'NavigatorItemWrapper noOfChildren',
  )

  const elementSupportsChildren = useEditorState(
    Substores.fullStore,
    // this is not good
    (store) => elementSupportsChildrenSelector(store, props.navigatorEntry),
    'NavigatorItemWrapper elementSupportsChildren',
  )

  const parentElement = useEditorState(
    Substores.metadata,
    (store) =>
      MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        EP.parentPath(props.navigatorEntry.elementPath),
      ),
    'NavigatorItemWrapper parentElement',
  )

  function isNullConditionalBranch(
    entry: NavigatorEntry,
    maybeConditional: JSXConditionalExpression | null,
  ) {
    if (maybeConditional == null) {
      return false
    }
    const truePath = EP.appendToPath(
      EP.parentPath(entry.elementPath),
      maybeConditional.whenTrue.uid,
    )
    const branch = EP.pathsEqual(entry.elementPath, truePath)
      ? maybeConditional.whenTrue
      : maybeConditional.whenFalse
    return isNullJSXAttributeValue(branch)
  }

  const canReparentInto =
    elementSupportsChildren ||
    isConditionalClauseNavigatorEntry(props.navigatorEntry) ||
    isNullConditionalBranch(props.navigatorEntry, maybeConditionalExpression(parentElement))

  const labelForTheElement = useEditorState(
    Substores.metadata,
    (store) => labelSelector(store, props.navigatorEntry),
    'NavigatorItemWrapper labelSelector',
  )
  const label = getNavigatorEntryLabel(props.navigatorEntry, labelForTheElement)

  const entryDepth = useEditorState(
    Substores.metadata,
    (store) => {
      return navigatorDepth(props.navigatorEntry, store.editor.jsxMetadata)
    },
    'NavigatorItemWrapper entryDepth',
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
          (isRegularNavigatorEntry(props.navigatorEntry) ||
            isConditionalClauseNavigatorEntry(props.navigatorEntry)) &&
          store.editor.navigator.dropTargetHint?.displayAtEntry != null &&
          navigatorEntriesEqual(
            store.editor.navigator.dropTargetHint.displayAtEntry,
            props.navigatorEntry,
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

  const navigatorItemProps: NavigatorItemDragAndDropWrapperPropsBase = {
    index: props.index,
    editorDispatch: dispatch,
    entryDepth: entryDepth,
    selected: isSelected,
    highlighted: isHighlighted,
    collapsed: isCollapsed,
    getCurrentlySelectedEntries: props.getCurrentlySelectedEntries,
    getSelectedViewsInRange: props.getSelectedViewsInRange,
    appropriateDropTargetHint: appropriateDropTargetHint,
    canReparentInto: canReparentInto,
    noOfChildren: noOfChildren,
    label: label,
    isElementVisible: isElementVisible,
    renamingTarget: renamingTarget,
    windowStyle: props.windowStyle,
    visibleNavigatorTargets: visibleNavigatorTargets,
  }

  if (props.navigatorEntry.type === 'REGULAR') {
    const entryProps: NavigatorItemDragAndDropWrapperProps = {
      ...navigatorItemProps,
      elementPath: props.navigatorEntry.elementPath,
    }
    return <NavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'SYNTHETIC') {
    const entryProps: SyntheticNavigatorItemContainerProps = {
      ...navigatorItemProps,
      childOrAttribute: props.navigatorEntry.childOrAttribute,
      elementPath: props.navigatorEntry.elementPath,
    }
    return <SyntheticNavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'CONDITIONAL_CLAUSE') {
    const entryProps: ConditionalClauseNavigatorItemContainerProps = {
      ...navigatorItemProps,
      navigatorEntry: props.navigatorEntry,
    }
    return <ConditionalClauseNavigatorItemContainer {...entryProps} />
  }

  assertNever(props.navigatorEntry)
})
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
