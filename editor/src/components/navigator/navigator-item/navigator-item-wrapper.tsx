/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { assertNever } from '../../../core/shared/utils'
import { createCachedSelector } from 're-reselect'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  childOrBlockIsChild,
  ElementInstanceMetadata,
  getJSXElementNameLastPart,
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
  navigatorEntryToKey,
} from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { DerivedSubstate, MetadataSubstate } from '../../editor/store/store-hook-substore-types'
import {
  DragSelection,
  NavigatorItemContainer,
  NavigatorItemDragAndDropWrapperProps,
} from './navigator-item-dnd-container'
import { jsxSimpleAttributeToValue } from '../../../core/shared/jsx-attributes'
import { foldEither } from '../../../core/shared/either'
import { navigatorDepth } from '../navigator-utils'

interface NavigatorItemWrapperProps {
  index: number
  targetComponentKey: string
  navigatorEntry: NavigatorEntry
  getDragSelections: () => Array<DragSelection>
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

const canBeReparentedIntoSelector = createCachedSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  targetElementMetadataSelector,
  targetInNavigatorItemsSelector,
  (projectContents, elementMetadata, elementInNavigatorTargets) => {
    if (!elementInNavigatorTargets || elementMetadata == null) {
      return false
    }
    return MetadataUtils.targetElementSupportsChildren(projectContents, elementMetadata)
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

const labelSelector = createCachedSelector(
  targetElementMetadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (elementMetadata, allElementProps) => {
    if (elementMetadata == null) {
      return 'Element 👻'
    }
    return MetadataUtils.getElementLabelFromMetadata(allElementProps, elementMetadata)
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

const elementWarningsSelector = createCachedSelector(
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
    case 'SYNTHETIC': {
      if (childOrBlockIsChild(navigatorEntry.childOrAttribute)) {
        switch (navigatorEntry.childOrAttribute.type) {
          case 'JSX_ELEMENT':
            return getJSXElementNameLastPart(navigatorEntry.childOrAttribute.name)
          case 'JSX_ARBITRARY_BLOCK':
            return 'Unknown'
          case 'JSX_TEXT_BLOCK':
            return navigatorEntry.childOrAttribute.text
          case 'JSX_FRAGMENT':
            return 'Fragment'
          case 'JSX_CONDITIONAL_EXPRESSION':
            return 'Conditional'
          default:
            throw assertNever(navigatorEntry.childOrAttribute)
        }
      } else {
        const simpleAttributeValue = jsxSimpleAttributeToValue(navigatorEntry.childOrAttribute)
        return foldEither(
          () => 'Unknown',
          (value) => {
            if (value === null) {
              return 'null'
            } else if (value === undefined) {
              return 'undefined'
            } else {
              return value.toString()
            }
          },
          simpleAttributeValue,
        )
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
      return noOfChildrenSelector(store, props.navigatorEntry)
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

  const entryDepth = useEditorState(
    Substores.metadata,
    (store) => {
      return navigatorDepth(props.navigatorEntry, store.editor.jsxMetadata)
    },
    'NavigatorItemWrapper entryDepth',
  )

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
    entryDepth: entryDepth,
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
