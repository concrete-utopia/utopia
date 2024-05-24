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
import { Icons, Tooltip, useColorTheme } from '../../../uuiui'
import { getRouteComponentNameForOutlet } from '../../canvas/remix/remix-utils'
import {
  selectComponents,
  setHighlightedViews,
  toggleCollapse,
} from '../../editor/actions/action-creators'
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
import type { CondensedNavigatorRow, CondensedNavigatorRowVariant } from '../navigator-row'
import { isRegulaNavigatorRow, type NavigatorRow } from '../navigator-row'
import { navigatorDepth } from '../navigator-utils'
import { LayoutIcon } from './layout-icon'
import { BasePaddingUnit, elementWarningsSelector } from './navigator-item'
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
import { ExpandableIndicator } from './expandable-indicator'
import { unless, when } from '../../../utils/react-conditionals'
import { DataReferenceCartoucheControl } from '../../inspector/sections/component-section/data-reference-cartouche'

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

const CondensedEntryItemWrapper = React.memo(
  (props: { windowStyle: React.CSSProperties; navigatorRow: CondensedNavigatorRow }) => {
    const colorTheme = useColorTheme()

    const selectedViews = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'CondensedEntryItemWrapper selectedViews',
    )

    const hasSelection = React.useMemo(() => {
      return selectedViews.some((path) =>
        props.navigatorRow.entries.some(
          (entry) => entry.type !== 'DATA_REFERENCE' && EP.pathsEqual(path, entry.elementPath),
        ),
      )
    }, [selectedViews, props.navigatorRow])

    const wholeRowInsideSelection = React.useMemo(() => {
      return selectedViews.some((path) =>
        props.navigatorRow.entries.every((entry) => EP.isDescendantOf(entry.elementPath, path)),
      )
    }, [selectedViews, props.navigatorRow])

    return (
      <div
        style={{
          ...props.windowStyle,
          display: 'flex',
          alignItems: 'center',
          backgroundColor:
            hasSelection || wholeRowInsideSelection
              ? colorTheme.childSelectionBlue.value
              : 'transparent',
          borderTopLeftRadius: wholeRowInsideSelection ? 0 : 5,
          borderTopRightRadius: 5,
          overflowX: 'auto',
        }}
      >
        {props.navigatorRow.entries.map((entry, idx) => {
          const showSeparator = idx < props.navigatorRow.entries.length - 1
          const separator = showSeparator ? (
            <CondensedEntryItemSeparator variant={props.navigatorRow.variant} />
          ) : null

          return (
            <CondensedEntryItem
              navigatorRow={props.navigatorRow}
              showExpandableIndicator={idx === 0}
              key={EP.toString(entry.elementPath)}
              entry={entry}
              separator={separator}
              wholeRowInsideSelection={wholeRowInsideSelection}
            />
          )
        })}
      </div>
    )
  },
)
CondensedEntryItemWrapper.displayName = 'CondensedEntryItemWrapper'

const CondensedEntryItemSeparator = React.memo(
  (props: { variant: CondensedNavigatorRowVariant }) => {
    const colorTheme = useColorTheme()

    if (props.variant === 'leaf') {
      return <div style={{ width: 5 }} />
    }

    return (
      <div
        style={{
          width: 12,
          height: 12,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          color: colorTheme.fg6.value,
        }}
      >
        <Icons.NarrowExpansionArrowRight />
      </div>
    )
  },
)

CondensedEntryItemSeparator.displayName = 'CondensedEntryItemSeparator'

const CondensedEntryItem = React.memo(
  (props: {
    entry: NavigatorEntry
    navigatorRow: CondensedNavigatorRow
    wholeRowInsideSelection: boolean
    separator: React.ReactNode
    showExpandableIndicator: boolean
  }) => {
    const colorTheme = useColorTheme()
    const dispatch = useDispatch()

    const selectedViews = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'CondensedEntry selectedViews',
    )

    const highlightedViews = useEditorState(
      Substores.highlightedHoveredViews,
      (store) => store.editor.highlightedViews,
      'CondensedEntry highlightedViews',
    )

    const iconOverride = useEditorState(
      Substores.propertyControlsInfo,
      (store) =>
        MetadataUtils.getIconOfComponent(
          props.entry.elementPath,
          store.editor.propertyControlsInfo,
          store.editor.projectContents,
        ),
      'CondensedEntry iconOverride',
    )

    const labelForTheElement = useEditorState(
      Substores.projectContentsAndMetadata,
      (store) => labelSelector(store, props.entry),
      'CondensedEntry labelSelector',
    )

    const elementWarnings = useEditorState(
      Substores.derived,
      (store) => elementWarningsSelector(store, props.entry),
      'CondensedEntry elementWarningsSelector',
    )

    const isCollapsed = useEditorState(
      Substores.navigator,
      (store) =>
        store.editor.navigator.collapsedViews.some((path) =>
          EP.pathsEqual(path, props.entry.elementPath),
        ),
      'CondensedEntryItemWrapper isCollapsed',
    )

    const isDataReference = React.useMemo(() => {
      return props.entry.type === 'DATA_REFERENCE'
    }, [props.entry])

    const showLabel = useEditorState(
      Substores.metadata,
      (store) => {
        return (
          MetadataUtils.isProbablyScene(store.editor.jsxMetadata, props.entry.elementPath) ||
          MetadataUtils.isProbablyRemixScene(store.editor.jsxMetadata, props.entry.elementPath)
        )
      },
      'CondensedEntryItemWrapper isScene',
    )

    const entriesBeforeMe = React.useMemo(() => {
      let entries: NavigatorEntry[] = []
      for (const entry of props.navigatorRow.entries) {
        if (EP.pathsEqual(entry.elementPath, props.entry.elementPath)) {
          break
        }
        entries.push(entry)
      }
      return entries
    }, [props.entry, props.navigatorRow])

    const isChildOfSelected = React.useMemo(() => {
      return selectedViews.some((path) =>
        entriesBeforeMe.some((other) => EP.pathsEqual(other.elementPath, path)),
      )
    }, [selectedViews, entriesBeforeMe])

    const selectionIsDataReference = React.useMemo(() => {
      const entries = props.navigatorRow.entries.filter((entry) =>
        selectedViews.some((path) => EP.pathsEqual(path, entry.elementPath)),
      )
      return entries.some((entry) => entry.type === 'DATA_REFERENCE')
    }, [props.navigatorRow, selectedViews])

    const isSelected = React.useMemo(() => {
      return selectedViews.some((path) => EP.pathsEqual(path, props.entry.elementPath))
    }, [selectedViews, props.entry])

    const onClick = React.useCallback(
      (e: React.MouseEvent) => {
        e.preventDefault()
        e.stopPropagation()
        dispatch([selectComponents([props.entry.elementPath], false)])
      },
      [dispatch, props.entry],
    )

    const onMouseOver = React.useCallback(() => {
      dispatch([setHighlightedViews([props.entry.elementPath])])
    }, [props.entry, dispatch])

    const onMouseOut = React.useCallback(() => {
      dispatch([
        setHighlightedViews(
          highlightedViews.filter((path) => !EP.pathsEqual(path, props.entry.elementPath)),
        ),
      ])
    }, [props.entry, dispatch, highlightedViews])

    const collapse = React.useCallback(
      (elementPath: ElementPath) => (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
        e.stopPropagation()
        dispatch([toggleCollapse(elementPath)], 'leftpane')
      },
      [dispatch],
    )

    const entryLabel = React.useMemo(() => {
      return getNavigatorEntryLabel(props.entry, labelForTheElement)
    }, [props.entry, labelForTheElement])

    return (
      <React.Fragment>
        <Tooltip title={entryLabel} disabled={isDataReference}>
          <div
            style={{
              minWidth: 29,
              height: 29,
              display: 'flex',
              flexShrink: 0,
              alignItems: 'center',
              justifyContent: 'center',
              backgroundColor:
                !props.wholeRowInsideSelection && !isChildOfSelected
                  ? colorTheme.bg1.value
                  : undefined,
              borderTopRightRadius: isSelected ? 5 : 0,
              borderBottomRightRadius: isSelected ? 5 : 0,
              paddingLeft: props.showExpandableIndicator
                ? BasePaddingUnit * props.navigatorRow.indentation
                : 0,
            }}
            onClick={onClick}
            onMouseOver={onMouseOver}
            onMouseOut={onMouseOut}
          >
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                gap: 5,
                borderRadius: 5,
                backgroundColor:
                  isSelected && !isDataReference ? colorTheme.selectionBlue.value : undefined,
                width: '100%',
                height: '100%',
                padding: props.showExpandableIndicator ? '0px 5px' : 0,
              }}
            >
              {when(
                props.showExpandableIndicator,
                <div
                  style={{
                    width: 12,
                    height: 29,
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                    cursor: 'pointer',
                  }}
                  onClick={collapse(props.entry.elementPath)}
                >
                  <ExpandableIndicator
                    visible={true}
                    collapsed={isCollapsed}
                    selected={false}
                    iconColor={isSelected ? 'white' : 'main'}
                  />
                </div>,
              )}
              {unless(
                isDataReference,
                <LayoutIcon
                  navigatorEntry={props.entry}
                  override={iconOverride}
                  color={isSelected ? 'white' : 'main'}
                  elementWarnings={elementWarnings}
                />,
              )}
              {when(
                showLabel,
                <span style={{ color: isSelected ? 'white' : undefined, padding: '0 4px' }}>
                  {entryLabel}
                </span>,
              )}
              {when(
                isDataReference,
                props.entry.type === 'DATA_REFERENCE' && (
                  <DataReferenceCartoucheControl selected={isSelected} {...props.entry} />
                ),
              )}
            </div>
          </div>
        </Tooltip>
        <div
          style={{
            backgroundColor: props.wholeRowInsideSelection
              ? 'transparent'
              : !selectionIsDataReference && (isSelected || isChildOfSelected)
              ? colorTheme.childSelectionBlue.value
              : colorTheme.bg1.value,
            height: '100%',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          {props.separator}
        </div>
      </React.Fragment>
    )
  },
)
CondensedEntryItem.displayName = 'CondensedEntryItem'

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
    type: NavigatorItemDragType,
    index: props.index,
    indentation: props.indentation,
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
