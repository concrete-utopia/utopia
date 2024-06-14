/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import { createCachedSelector } from 're-reselect'
import React from 'react'
import { maybeConditionalExpression } from '../../../core/model/conditionals'
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
import { assertNever } from '../../../core/shared/utils'
import { getRouteComponentNameForOutlet } from '../../canvas/remix/remix-utils'
import { useDispatch } from '../../editor/store/dispatch-context'
import type {
  DropTargetHint,
  EditorStorePatched,
  NavigatorEntry,
} from '../../editor/store/editor-state'
import {
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  navigatorEntriesEqual,
  navigatorEntryToKey,
} from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type {
  DerivedSubstate,
  MetadataSubstate,
  ProjectContentAndMetadataSubstate,
  PropertyControlsInfoSubstate,
} from '../../editor/store/store-hook-substore-types'
import { isRegulaNavigatorRow, type NavigatorRow } from '../navigator-row'
import type {
  ConditionalClauseNavigatorItemContainerProps,
  ErrorNavigatorItemContainerProps,
  NavigatorItemDragAndDropWrapperProps,
  NavigatorItemDragAndDropWrapperPropsBase,
  RenderPropNavigatorItemContainerProps,
  SlotNavigatorItemContainerProps,
  SyntheticNavigatorItemContainerProps,
} from './navigator-item-dnd-container'
import {
  ConditionalClauseNavigatorItemContainer,
  ErrorNavigatorItemContainer,
  NavigatorItemContainer,
  NavigatorItemDragType,
  RenderPropNavigatorItemContainer,
  SlotNavigatorItemContainer,
  SyntheticNavigatorItemContainer,
} from './navigator-item-dnd-container'
import { CondensedEntryItemWrapper } from './navigator-condensed-entry'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  navigatorRow: NavigatorRow
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
  (store: PropertyControlsInfoSubstate) => store.editor.propertyControlsInfo,
  (
    projectContents,
    metadata,
    elementMetadata,
    pathTrees,
    elementInNavigatorTargets,
    propertyControlsInfo,
  ) => {
    if (!elementInNavigatorTargets || elementMetadata == null) {
      return false
    }
    return MetadataUtils.targetElementSupportsChildren(
      projectContents,
      elementMetadata.elementPath,
      metadata,
      pathTrees,
      propertyControlsInfo,
    )
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

export const labelSelector = createCachedSelector(
  (store: ProjectContentAndMetadataSubstate) => store.editor.jsxMetadata,
  targetElementMetadataSelector,
  (store: ProjectContentAndMetadataSubstate) => store.editor.allElementProps,
  (store: ProjectContentAndMetadataSubstate) => store.editor.elementPathTree,
  (store: ProjectContentAndMetadataSubstate) => store.editor.projectContents,
  (metadata, elementMetadata, allElementProps, pathTrees, projectContents) => {
    if (elementMetadata == null) {
      // "Element" with ghost emoji.
      return 'Element 👻'
    }
    const label = MetadataUtils.getElementLabelFromMetadata(
      metadata,
      allElementProps,
      pathTrees,
      elementMetadata,
    )

    const routeComponentName = getRouteComponentNameForOutlet(
      elementMetadata.elementPath,
      metadata,
      projectContents,
      pathTrees,
    )

    return routeComponentName == null ? label : `${label}: ${routeComponentName}`
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
    case 'DATA_REFERENCE':
    case 'SYNTHETIC': {
      switch (navigatorEntry.childOrAttribute.type) {
        case 'JSX_ELEMENT':
          return getJSXElementNameLastPart(navigatorEntry.childOrAttribute.name)
        case 'JSX_MAP_EXPRESSION':
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
        case 'JS_IDENTIFIER':
          return '(code)'
        case 'JS_ELEMENT_ACCESS':
          return '(code)'
        case 'JS_PROPERTY_ACCESS':
          return '(code)'
        default:
          throw assertNever(navigatorEntry.childOrAttribute)
      }
    }
    case 'RENDER_PROP':
      return navigatorEntry.propName
    case 'RENDER_PROP_VALUE':
      return labelForTheElement
    case 'INVALID_OVERRIDE':
      return navigatorEntry.message
    case 'SLOT':
      return '(empty)'
    default:
      assertNever(navigatorEntry)
  }
}

export const NavigatorItemWrapper: React.FunctionComponent<NavigatorItemWrapperProps> = React.memo(
  (props) => {
    if (isRegulaNavigatorRow(props.navigatorRow)) {
      const navigatorEntry = props.navigatorRow.entry
      return (
        <SingleEntryNavigatorItemWrapper
          index={props.index}
          indentation={props.navigatorRow.indentation}
          targetComponentKey={props.targetComponentKey}
          navigatorRow={props.navigatorRow}
          getCurrentlySelectedEntries={props.getCurrentlySelectedEntries}
          getSelectedViewsInRange={props.getSelectedViewsInRange}
          windowStyle={props.windowStyle}
          navigatorEntry={navigatorEntry}
        />
      )
    }
    return (
      <CondensedEntryItemWrapper
        windowStyle={props.windowStyle}
        navigatorRow={props.navigatorRow}
      />
    )
  },
)

type SingleEntryNavigatorItemWrapperProps = NavigatorItemWrapperProps & {
  indentation: number
  navigatorEntry: NavigatorEntry
}

const SingleEntryNavigatorItemWrapper: React.FunctionComponent<
  React.PropsWithChildren<SingleEntryNavigatorItemWrapperProps>
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

  const maybeParentConditional = useEditorState(
    Substores.metadata,
    (store) =>
      maybeConditionalExpression(
        MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          EP.parentPath(props.navigatorEntry.elementPath),
        ),
      ),
    'NavigatorItemWrapper maybeParentConditional',
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
    isNullConditionalBranch(props.navigatorEntry, maybeParentConditional)

  const labelForTheElement = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => labelSelector(store, props.navigatorEntry),
    'NavigatorItemWrapper labelSelector',
  )
  const label = getNavigatorEntryLabel(props.navigatorEntry, labelForTheElement)

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
    type: NavigatorItemDragType,
    index: props.index,
    indentation: props.indentation,
    editorDispatch: dispatch,
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
    navigatorEntry: props.navigatorEntry,
  }

  if (props.navigatorEntry.type === 'REGULAR') {
    const entryProps: NavigatorItemDragAndDropWrapperProps = {
      ...navigatorItemProps,
      elementPath: props.navigatorEntry.elementPath,
    }
    return <NavigatorItemContainer {...entryProps} />
  }

  if (
    props.navigatorEntry.type === 'SYNTHETIC' ||
    props.navigatorEntry.type === 'DATA_REFERENCE' // TODO remove this once we have a proper navigator item container for data references
  ) {
    const entryProps: SyntheticNavigatorItemContainerProps = {
      ...navigatorItemProps,
      childOrAttribute: props.navigatorEntry.childOrAttribute,
      elementPath: props.navigatorEntry.elementPath,
      isOutletOrDescendantOfOutlet: false,
    }
    return <SyntheticNavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'CONDITIONAL_CLAUSE') {
    const entryProps: ConditionalClauseNavigatorItemContainerProps = {
      ...navigatorItemProps,
      navigatorEntry: props.navigatorEntry,
      isOutletOrDescendantOfOutlet: false,
    }
    return <ConditionalClauseNavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'INVALID_OVERRIDE') {
    const entryProps: ErrorNavigatorItemContainerProps = {
      ...navigatorItemProps,
      navigatorEntry: props.navigatorEntry,
      isOutletOrDescendantOfOutlet: false,
    }

    return <ErrorNavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'RENDER_PROP') {
    const entryProps: RenderPropNavigatorItemContainerProps = {
      ...navigatorItemProps,
      propName: props.navigatorEntry.propName,
      elementPath: props.navigatorEntry.elementPath,
      isOutletOrDescendantOfOutlet: false,
      childPath: props.navigatorEntry.childPath,
    }
    return <RenderPropNavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'RENDER_PROP_VALUE') {
    const entryProps: NavigatorItemDragAndDropWrapperProps = {
      ...navigatorItemProps,
      elementPath: props.navigatorEntry.elementPath,
    }
    return <NavigatorItemContainer {...entryProps} />
  }

  if (props.navigatorEntry.type === 'SLOT') {
    const entryProps: SlotNavigatorItemContainerProps = {
      ...navigatorItemProps,
      renderProp: props.navigatorEntry.prop,
      parentElementPath: props.navigatorEntry.elementPath,
    }
    return <SlotNavigatorItemContainer {...entryProps} />
  }

  assertNever(props.navigatorEntry)
})
NavigatorItemWrapper.displayName = 'NavigatorItemWrapper'
