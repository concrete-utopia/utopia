/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import type { ConditionalCase } from '../../../core/model/conditionals'
import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  getConditionalCaseCorrespondingToBranchPath,
  getConditionalClausePath,
  getConditionalFlag,
  isActiveBranchOfConditional,
  isDefaultBranchOfConditional,
  isOverriddenConditional,
  maybeConditionalActiveBranch,
  maybeConditionalExpression,
} from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { hasElementsWithin } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { unless, when } from '../../../utils/react-conditionals'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import type { IcnProps } from '../../../uuiui'
import { FlexRow, useColorTheme, UtopiaTheme } from '../../../uuiui'
import type { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { isEntryAConditionalSlot } from '../../canvas/canvas-utils'
import type { EditorAction, EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as MetaActions from '../../editor/actions/meta-actions'
import type { ElementWarnings, NavigatorEntry } from '../../editor/store/editor-state'
import {
  defaultElementWarnings,
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  isSyntheticNavigatorEntry,
  navigatorEntryToKey,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type {
  DerivedSubstate,
  MetadataSubstate,
} from '../../editor/store/store-hook-substore-types'
import { navigatorDepth } from '../navigator-utils'
import { ComponentPreview } from './component-preview'
import { ExpandableIndicator } from './expandable-indicator'
import { ItemLabel } from './item-label'
import { LayoutIcon } from './layout-icon'
import { NavigatorItemActionSheet } from './navigator-item-components'
import { assertNever } from '../../../core/shared/utils'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { invalidGroupStateToString } from '../../canvas/canvas-strategies/strategies/group-helpers'

export function getItemHeight(navigatorEntry: NavigatorEntry): number {
  if (isConditionalClauseNavigatorEntry(navigatorEntry)) {
    return UtopiaTheme.layout.rowHeight.smallest
  } else {
    // Default size for everything else.
    return UtopiaTheme.layout.rowHeight.smaller
  }
}

export const NavigatorItemTestId = (pathString: string): string =>
  `NavigatorItemTestId-${pathString}`

interface ComputedLook {
  style: React.CSSProperties & { '--iconOpacity'?: number }
  iconColor: IcnProps['color']
}

export const BasePaddingUnit = 20

export function getElementPadding(withNavigatorDepth: number): number {
  const paddingOffset = withNavigatorDepth - 1
  return paddingOffset * BasePaddingUnit
}

export type ParentOutline = 'solid' | 'child' | 'none'

export interface NavigatorItemInnerProps {
  navigatorEntry: NavigatorEntry
  index: number
  getSelectedViewsInRange: (i: number) => Array<ElementPath> // TODO KILLME
  noOfChildren: number
  label: string
  dispatch: EditorDispatch
  isHighlighted: boolean
  collapsed: boolean
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  selected: boolean
  parentOutline: ParentOutline
  visibleNavigatorTargets: Array<NavigatorEntry>
}

function getSelectionActions(
  getSelectedViewsInRange: (i: number) => Array<ElementPath>,
  index: number,
  elementPath: ElementPath,
  selected: boolean,
  event: React.MouseEvent<HTMLDivElement>,
): Array<EditorAction> {
  if (!selected) {
    if (event.metaKey && !event.shiftKey) {
      // adds to selection
      return MetaActions.selectComponents([elementPath], true)
    } else if (event.shiftKey) {
      // selects range of items
      const targets = getSelectedViewsInRange(index)
      return MetaActions.selectComponents(targets, false)
    } else {
      return MetaActions.selectComponents([elementPath], false)
    }
  } else {
    return []
  }
}

type ConditionalOverrideUpdate = ConditionalCase | 'clear-override' | 'no-update'

function getConditionalOverrideActions(
  targetPath: ElementPath,
  conditionalOverrideUpdate: ConditionalOverrideUpdate,
): Array<EditorAction> {
  switch (conditionalOverrideUpdate) {
    case 'no-update':
      return []
    case 'clear-override':
      return [EditorActions.setConditionalOverriddenCondition(targetPath, null)]
    case 'true-case':
      return [EditorActions.setConditionalOverriddenCondition(targetPath, true)]
    case 'false-case':
      return [EditorActions.setConditionalOverriddenCondition(targetPath, false)]
    default:
      assertNever(conditionalOverrideUpdate)
  }
}

function selectItem(
  dispatch: EditorDispatch,
  getSelectedViewsInRange: (i: number) => Array<ElementPath>,
  navigatorEntry: NavigatorEntry,
  index: number,
  selected: boolean,
  event: React.MouseEvent<HTMLDivElement>,
  conditionalOverrideUpdate: ConditionalOverrideUpdate,
) {
  const elementPath = navigatorEntry.elementPath
  const selectionActions = isConditionalClauseNavigatorEntry(navigatorEntry)
    ? []
    : getSelectionActions(getSelectedViewsInRange, index, elementPath, selected, event)

  const conditionalOverrideActions = isConditionalClauseNavigatorEntry(navigatorEntry)
    ? getConditionalOverrideActions(elementPath, conditionalOverrideUpdate)
    : getConditionalOverrideActions(EP.parentPath(elementPath), conditionalOverrideUpdate)

  dispatch([...conditionalOverrideActions, ...selectionActions], 'leftpane')
}

const highlightItem = (
  dispatch: EditorDispatch,
  elementPath: ElementPath,
  selected: boolean,
  highlighted: boolean,
) => {
  if (!highlighted) {
    if (selected) {
      dispatch([EditorActions.clearHighlightedViews()], 'leftpane')
    } else {
      dispatch([EditorActions.setHighlightedView(elementPath)], 'leftpane')
    }
  }
}

const collapseItem = (
  dispatch: EditorDispatch,
  elementPath: ElementPath,
  e: React.MouseEvent<HTMLDivElement>,
) => {
  dispatch([EditorActions.toggleCollapse(elementPath)], 'leftpane')
  e.stopPropagation()
}

const defaultUnselected = (colorTheme: ThemeObject): ComputedLook => ({
  style: { background: 'transparent', color: colorTheme.fg0.value },
  iconColor: 'main',
})

const defaultSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: { background: colorTheme.denimBlue.value, color: colorTheme.fg0.value },
  iconColor: 'main',
})

const erroredGroup = (colorTheme: ThemeObject, selected: boolean): ComputedLook => ({
  style: {
    color: colorTheme.error.value,
    background: selected ? defaultSelected(colorTheme).style.background : 'transparent',
  },
  iconColor: 'error',
})

const descendantOfSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: { background: colorTheme.lightDenimBlue.value, color: colorTheme.fg0.value },
  iconColor: 'main',
})

const dynamicUnselected = (colorTheme: ThemeObject): ComputedLook => ({
  style: { background: 'transparent', color: colorTheme.dynamicBlue.value },
  iconColor: 'dynamic',
})

const dynamicSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: { background: colorTheme.denimBlue.value, color: colorTheme.dynamicBlue.value },
  iconColor: 'dynamic',
})

const dynamicDescendantOfSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: { background: colorTheme.lightDenimBlue.value, color: colorTheme.dynamicBlue.value },
  iconColor: 'dynamic',
})

const componentUnselected = (colorTheme: ThemeObject): ComputedLook => ({
  style: {
    background: 'transparent',
    color: colorTheme.componentOrange.value,
  },
  iconColor: 'component-orange',
})

const componentSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: {
    background: colorTheme.denimBlue.value,
    color: colorTheme.componentOrange.value,
  },
  iconColor: 'component-orange',
})

const componentDescendantOfSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: {
    background: colorTheme.lightDenimBlue.value,
    color: colorTheme.componentOrange.value,
  },
  iconColor: 'component-orange',
})

const componentInstanceSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: {
    background: colorTheme.denimBlue.value,
    color: colorTheme.componentPurple.value,
  },
  iconColor: 'component',
})

const computeResultingStyle = (
  selected: boolean,
  isInsideComponent: boolean,
  isDynamic: boolean,
  isProbablyScene: boolean,
  fullyVisible: boolean,
  isFocusedComponent: boolean,
  isFocusableComponent: boolean,
  isHighlightedForInteraction: boolean,
  isDescendantOfSelected: boolean,
  isErroredGroup: boolean,
  colorTheme: ThemeObject,
) => {
  let result = defaultUnselected(colorTheme)
  if (isHighlightedForInteraction) {
    result = defaultSelected(colorTheme)
  } else if (isInsideComponent && isDescendantOfSelected) {
    result = componentDescendantOfSelected(colorTheme)
  } else if (isInsideComponent) {
    result = componentUnselected(colorTheme)
  } else if (isDynamic && isDescendantOfSelected) {
    result = dynamicDescendantOfSelected(colorTheme)
  } else if (isDynamic) {
    result = dynamicUnselected(colorTheme)
  } else if (isDescendantOfSelected) {
    result = descendantOfSelected(colorTheme)
  } else {
    result = defaultUnselected(colorTheme)
  }

  if (selected) {
    if (isFocusableComponent && !isFocusedComponent) {
      result = componentInstanceSelected(colorTheme)
    } else if (isInsideComponent) {
      result = componentSelected(colorTheme)
    } else if (isDynamic) {
      result = dynamicSelected(colorTheme)
    } else {
      result = defaultSelected(colorTheme)
    }
  }

  if (isErroredGroup) {
    result = erroredGroup(colorTheme, selected)
  }

  const isProbablyParentOfSelected = (isProbablyScene || fullyVisible) && !selected

  result.style = {
    ...result.style,
    fontWeight: isProbablyParentOfSelected || isProbablyScene ? 600 : 'inherit',
  }

  return result
}

function useStyleFullyVisible(
  navigatorEntry: NavigatorEntry,
  autoFocusedPaths: Array<ElementPath>,
): boolean {
  return useEditorState(
    Substores.metadata,
    (store) => {
      if (isRegularNavigatorEntry(navigatorEntry)) {
        const path = navigatorEntry.elementPath
        const metadata = store.editor.jsxMetadata
        const selectedViews = store.editor.selectedViews
        const isSelected = selectedViews.some((selected) => EP.pathsEqual(path, selected))
        const isParentOfSelected = selectedViews.some((selected) => EP.isParentOf(path, selected))

        const isStoryboardChild = EP.isStoryboardChild(path)

        const isContainingBlockAncestor = selectedViews.some((selected) => {
          return EP.pathsEqual(MetadataUtils.findContainingBlock(metadata, selected), path)
        })

        const isFlexAncestorDirectionChange = selectedViews.some((selected) => {
          const selectedSizeMeasurements = MetadataUtils.findElementByElementPath(
            metadata,
            selected,
          )?.specialSizeMeasurements
          const parentPath = EP.parentPath(selected)
          if (
            selectedSizeMeasurements?.parentLayoutSystem === 'flex' &&
            !isParentOfSelected &&
            EP.isDescendantOfOrEqualTo(selected, path) &&
            parentPath != null
          ) {
            const flexDirectionChange = MetadataUtils.findNearestAncestorFlexDirectionChange(
              metadata,
              parentPath,
            )
            return EP.pathsEqual(flexDirectionChange, path)
          } else {
            return false
          }
        })

        let isInsideFocusedComponent =
          EP.isFocused(store.editor.focusedElementPath, path) ||
          EP.isInsideFocusedComponent(path, autoFocusedPaths)

        return (
          isStoryboardChild ||
          isSelected ||
          isParentOfSelected ||
          isContainingBlockAncestor ||
          isFlexAncestorDirectionChange ||
          isInsideFocusedComponent
        )
      } else {
        return false
      }
    },
    'NavigatorItem useStyleFullyVisible',
  )
}

function useIsProbablyScene(navigatorEntry: NavigatorEntry): boolean {
  return useEditorState(
    Substores.metadata,
    (store) =>
      isRegularNavigatorEntry(navigatorEntry) &&
      MetadataUtils.isProbablyScene(store.editor.jsxMetadata, navigatorEntry.elementPath),
    'NavigatorItem useIsProbablyScene',
  )
}

const isHiddenConditionalBranchSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate, _elementPath: ElementPath, parentPath: ElementPath) =>
    MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, parentPath),
  (_store: MetadataSubstate, elementPath: ElementPath, _parentPath: ElementPath) => elementPath,
  (_store: MetadataSubstate, _elementPath: ElementPath, parentPath: ElementPath) => parentPath,
  (
    metadata: ElementInstanceMetadataMap,
    parent: ElementInstanceMetadata | null,
    elementPath: ElementPath,
    parentPath: ElementPath,
  ): boolean => {
    if (parent == null) {
      return false
    }
    const originalConditionValue = parent.conditionValue
    if (originalConditionValue === 'not-a-conditional') {
      return false
    }

    const conditional = maybeConditionalExpression(parent)
    if (conditional == null) {
      return false
    }

    const flag = getConditionalFlag(conditional)

    // the final condition value, either from the original or from the override
    const overriddenConditionValue: boolean = flag ?? originalConditionValue.active

    const branch = maybeConditionalActiveBranch(parentPath, metadata)

    if (
      EP.isDynamicPath(elementPath) &&
      hasElementsWithin(branch) &&
      Object.values(branch.elementsWithin)
        .map((e) => EP.appendToPath(parentPath, e.uid))
        .find((e) => EP.pathsEqual(e, EP.dynamicPathToStaticPath(elementPath))) != null
    ) {
      // the branch is active _and_ it's dynamic
      return false
    }

    // when the condition is true, then the 'then' branch is not hidden
    if (overriddenConditionValue) {
      const trueClausePath = getConditionalClausePath(parentPath, conditional.whenTrue)
      return !EP.pathsEqual(elementPath, trueClausePath)
    }
    // when the condition is false, then the 'else' branch is not hidden
    const falseClausePath = getConditionalClausePath(parentPath, conditional.whenFalse)
    return !EP.pathsEqual(elementPath, falseClausePath)
  },
)((_, elementPath, parentPath) => `${EP.toString(elementPath)}_${EP.toString(parentPath)}`)

const elementWarningsSelector = createCachedSelector(
  (store: DerivedSubstate) => store.derived.elementWarnings,
  (_: DerivedSubstate, navigatorEntry: NavigatorEntry) => navigatorEntry,
  (elementWarnings, navigatorEntry) => {
    if (isRegularNavigatorEntry(navigatorEntry)) {
      return elementWarnings[EP.toString(navigatorEntry.elementPath)] ?? defaultElementWarnings
    } else {
      return defaultElementWarnings
    }
  },
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

export const NavigatorItem: React.FunctionComponent<
  React.PropsWithChildren<NavigatorItemInnerProps>
> = React.memo((props) => {
  const {
    dispatch,
    isHighlighted,
    isElementVisible,
    selected,
    collapsed,
    navigatorEntry,
    getSelectedViewsInRange,
    index,
  } = props

  const colorTheme = useColorTheme()

  const autoFocusedPaths = useEditorState(
    Substores.derived,
    (store) => store.derived.autoFocusedPaths,
    'NavigatorItem autoFocusedPaths',
  )

  const isFocusedComponent = useEditorState(
    Substores.focusedElement,
    (store) =>
      isRegularNavigatorEntry(navigatorEntry) &&
      EP.isExplicitlyFocused(
        store.editor.focusedElementPath,
        autoFocusedPaths,
        navigatorEntry.elementPath,
      ),
    'NavigatorItem isFocusedComponent',
  )

  const elementWarnings = useEditorState(
    Substores.derived,
    (store) => elementWarningsSelector(store, props.navigatorEntry),
    'NavigatorItem elementWarningsSelector',
  )

  const isManuallyFocusableComponent = useEditorState(
    Substores.metadata,
    (store) => {
      return (
        isRegularNavigatorEntry(navigatorEntry) &&
        MetadataUtils.isManuallyFocusableComponent(
          navigatorEntry.elementPath,
          store.editor.jsxMetadata,
          autoFocusedPaths,
        )
      )
    },
    'NavigatorItem isManuallyFocusableComponent',
  )

  const entryNavigatorDepth = useEditorState(
    Substores.metadata,
    (store) => {
      return navigatorDepth(navigatorEntry, store.editor.jsxMetadata)
    },
    'NavigatorItem entryNavigatorDepth',
  )

  const containsExpressions: boolean = useEditorState(
    Substores.metadata,
    (store) => {
      return elementContainsExpressions(
        navigatorEntry.elementPath,
        store.editor.jsxMetadata,
        store.editor.elementPathTree,
      )
    },
    'NavigatorItem entryNavigatorDepth',
  )

  const isConditionalDynamicBranch = useEditorState(
    Substores.metadata,
    (store) => {
      if (
        !isSyntheticNavigatorEntry(navigatorEntry) ||
        !hasElementsWithin(navigatorEntry.childOrAttribute)
      ) {
        return false
      }

      const parentPath = EP.parentPath(navigatorEntry.elementPath)
      const conditionalParent = findMaybeConditionalExpression(parentPath, store.editor.jsxMetadata)
      if (conditionalParent == null) {
        return false
      }

      const clause = getConditionalCaseCorrespondingToBranchPath(
        getConditionalClausePath(parentPath, navigatorEntry.childOrAttribute),
        store.editor.jsxMetadata,
      )
      if (clause == null) {
        return false
      }

      return (
        getConditionalActiveCase(parentPath, conditionalParent, store.editor.spyMetadata) === clause
      )
    },
    'NavigatorItem isConditionalDynamicBranch',
  )

  const childComponentCount = props.noOfChildren

  const isGenerated = MetadataUtils.isElementGenerated(navigatorEntry.elementPath)
  const isDynamic = isGenerated || containsExpressions || isConditionalDynamicBranch
  const isConditional = useEditorState(
    Substores.metadata,
    (store) => {
      if (isRegularNavigatorEntry(navigatorEntry)) {
        return MetadataUtils.isElementPathConditionalFromMetadata(
          store.editor.jsxMetadata,
          navigatorEntry.elementPath,
        )
      } else {
        return false
      }
    },
    'NavigatorItem isConditional',
  )

  const conditionalOverrideUpdate = useEditorState(
    Substores.metadata,
    (store): ConditionalOverrideUpdate => {
      const path = navigatorEntry.elementPath
      const metadata = store.editor.jsxMetadata
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        navigatorEntry.elementPath,
      )
      if (isConditionalClauseNavigatorEntry(navigatorEntry)) {
        if (isActiveBranchOfConditional(navigatorEntry.clause, elementMetadata)) {
          if (isOverriddenConditional(elementMetadata)) {
            return 'clear-override'
          } else {
            return navigatorEntry.clause
          }
        } else {
          return navigatorEntry.clause
        }
      } else {
        const conditionalCase = getConditionalCaseCorrespondingToBranchPath(path, metadata)
        if (conditionalCase != null) {
          const parentPath = EP.parentPath(path)
          const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parentPath)
          if (isActiveBranchOfConditional(conditionalCase, parentMetadata)) {
            return 'no-update'
          } else if (isDefaultBranchOfConditional(conditionalCase, parentMetadata)) {
            return 'clear-override'
          } else {
            return conditionalCase
          }
        }

        return 'no-update'
      }
    },
    'NavigatorItem conditionalOverrideUpdate',
  )

  const isInsideComponent =
    EP.isInsideFocusedComponent(navigatorEntry.elementPath, autoFocusedPaths) || isFocusedComponent
  const fullyVisible = useStyleFullyVisible(navigatorEntry, autoFocusedPaths)
  const isProbablyScene = useIsProbablyScene(navigatorEntry)

  const isHighlightedForInteraction = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return (
        isRegularNavigatorEntry(props.navigatorEntry) &&
        store.editor.navigator.highlightedTargets.some((target) =>
          EP.pathsEqual(target, props.navigatorEntry.elementPath),
        )
      )
    },
    'isreallyhighlighted',
  )

  const isDescendantOfSelected = useEditorState(
    Substores.selectedViews,
    (store) =>
      store.editor.selectedViews.some((path) =>
        EP.isDescendantOfOrEqualTo(navigatorEntry.elementPath, path),
      ),
    'navigator item isDescendantOfSelected',
  )

  const isErroredGroup = React.useMemo(() => {
    return elementWarnings.invalidGroup != null || elementWarnings.invalidGroupChild != null
  }, [elementWarnings])

  const resultingStyle = computeResultingStyle(
    selected,
    isInsideComponent,
    isDynamic,
    isProbablyScene,
    fullyVisible,
    isFocusedComponent,
    isManuallyFocusableComponent,
    isHighlightedForInteraction,
    isDescendantOfSelected,
    isErroredGroup,
    colorTheme,
  )

  const collapse = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
      collapseItem(dispatch, navigatorEntry.elementPath, event)
      event.stopPropagation()
    },
    [dispatch, navigatorEntry.elementPath],
  )
  const select = React.useCallback(
    (event: any) =>
      selectItem(
        dispatch,
        getSelectedViewsInRange,
        navigatorEntry,
        index,
        selected,
        event,
        conditionalOverrideUpdate,
      ),
    [dispatch, getSelectedViewsInRange, navigatorEntry, index, selected, conditionalOverrideUpdate],
  )
  const highlight = React.useCallback(
    () => highlightItem(dispatch, navigatorEntry.elementPath, selected, isHighlighted),
    [dispatch, navigatorEntry.elementPath, selected, isHighlighted],
  )
  const focusComponent = React.useCallback(
    (event: React.MouseEvent) => {
      if (isManuallyFocusableComponent && !event.altKey) {
        dispatch([EditorActions.setFocusedElement(navigatorEntry.elementPath)])
      }
    },
    [dispatch, navigatorEntry.elementPath, isManuallyFocusableComponent],
  )

  const isHiddenConditionalBranch = useEditorState(
    Substores.metadata,
    (store) =>
      isHiddenConditionalBranchSelector(
        store,
        props.navigatorEntry.elementPath,
        EP.parentPath(props.navigatorEntry.elementPath),
      ),
    'NavigatorItem isHiddenConditionalBranch',
  )

  const isSlot = useEditorState(
    Substores.metadata,
    (store) => isEntryAConditionalSlot(store.editor.jsxMetadata, props.navigatorEntry),
    'NavigatorItem parentElement',
  )

  const containerStyle: React.CSSProperties = React.useMemo(() => {
    return {
      opacity: isElementVisible && (!isHiddenConditionalBranch || isSlot) ? undefined : 0.4,
      overflow: 'hidden',
      flexGrow: 1,
      flexShrink: 0,
    }
  }, [isElementVisible, isHiddenConditionalBranch, isSlot])

  const cursorStyle = isConditionalClauseNavigatorEntry(navigatorEntry) ? { cursor: 'pointer' } : {}

  const rowStyle = useKeepReferenceEqualityIfPossible({
    paddingLeft: getElementPadding(entryNavigatorDepth),
    height: getItemHeight(navigatorEntry),
    ...resultingStyle.style,
    ...cursorStyle,
  })

  const showExpandableIndicator = React.useMemo(() => {
    return (
      isConditional || // if it is a conditional, so it could have no children if both branches are null
      childComponentCount > 0 ||
      isFocusedComponent
    )
  }, [childComponentCount, isFocusedComponent, isConditional])

  return (
    <div
      style={{
        outline: `1px solid ${
          props.parentOutline === 'solid'
            ? colorTheme.navigatorResizeHintBorder.value
            : 'transparent'
        }`,
        outlineOffset: props.parentOutline === 'solid' ? '-1px' : 0,
      }}
    >
      <FlexRow
        data-testid={NavigatorItemTestId(varSafeNavigatorEntryToKey(navigatorEntry))}
        style={rowStyle}
        onMouseDown={select}
        onMouseMove={highlight}
        onDoubleClick={focusComponent}
      >
        <FlexRow style={containerStyle}>
          <ExpandableIndicator
            key='expandable-indicator'
            visible={showExpandableIndicator}
            collapsed={collapsed}
            selected={selected && !isInsideComponent}
            onMouseDown={collapse}
            style={{ transform: 'scale(0.6)', opacity: 'var(--paneHoverOpacity)' }}
            testId={`navigator-item-collapse-${navigatorEntryToKey(props.navigatorEntry)}`}
            iconColor={isConditional ? 'dynamic' : resultingStyle.iconColor}
          />
          <NavigatorRowLabel
            shouldShowParentOutline={props.parentOutline === 'child'}
            navigatorEntry={navigatorEntry}
            label={props.label}
            renamingTarget={props.renamingTarget}
            selected={props.selected}
            dispatch={props.dispatch}
            isDynamic={isDynamic}
            iconColor={isConditional ? 'dynamic' : resultingStyle.iconColor}
            elementWarnings={!isConditional ? elementWarnings : null}
            isSlot={isSlot}
          />
        </FlexRow>
        <NavigatorItemActionSheet
          navigatorEntry={navigatorEntry}
          selected={selected}
          highlighted={isHighlighted}
          isVisibleOnCanvas={isElementVisible}
          instanceOriginalComponentName={null}
          dispatch={dispatch}
          isSlot={isSlot}
          iconColor={isConditional ? 'dynamic' : resultingStyle.iconColor}
        />
      </FlexRow>
    </div>
  )
})
NavigatorItem.displayName = 'NavigatorItem'

interface NavigatorRowLabelProps {
  navigatorEntry: NavigatorEntry
  iconColor: IcnProps['color']
  elementWarnings: ElementWarnings | null
  label: string
  isDynamic: boolean
  renamingTarget: ElementPath | null
  selected: boolean
  shouldShowParentOutline: boolean
  isSlot: boolean
  dispatch: EditorDispatch
}

export const NavigatorRowLabel = React.memo((props: NavigatorRowLabelProps) => {
  const colorTheme = useColorTheme()

  const isConditionalLabel = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isRegularNavigatorEntry(props.navigatorEntry)) {
        return false
      }
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.navigatorEntry.elementPath,
      )
      const conditional = maybeConditionalExpression(elementMetadata)
      return conditional != null
    },
    'NavigatorRowLabel isConditionalLabel',
  )

  return (
    <React.Fragment>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          justifyContent: 'flex-start',
          gap: 10,
          borderRadius: 20,
          height: 22,
          padding: '0 15px 0 10px',
          backgroundColor:
            isConditionalLabel && !props.selected ? colorTheme.dynamicBlue10.value : 'transparent',
          color: isConditionalLabel ? colorTheme.dynamicBlue.value : undefined,
          textTransform: isConditionalLabel ? 'uppercase' : undefined,
        }}
      >
        {when(
          props.isSlot,
          <div
            key={`label-${props.label}-slot`}
            style={{
              width: '100%',
              height: 19,
              borderRadius: 20,
              padding: '0 40px',
              textAlign: 'center',
              textTransform: 'lowercase',
              backgroundColor: colorTheme.unavailable.value,
              color: props.shouldShowParentOutline
                ? colorTheme.navigatorResizeHintBorder.value
                : colorTheme.unavailableGrey10.value,
              border: `1px solid ${
                props.shouldShowParentOutline
                  ? colorTheme.navigatorResizeHintBorder.value
                  : colorTheme.unavailableGrey10.value
              }`,
            }}
          >
            Empty
          </div>,
        )}
        {unless(
          props.isSlot,
          <React.Fragment>
            {unless(
              props.navigatorEntry.type === 'CONDITIONAL_CLAUSE',
              <LayoutIcon
                key={`layout-type-${props.label}`}
                navigatorEntry={props.navigatorEntry}
                color={props.iconColor}
                elementWarnings={props.elementWarnings}
              />,
            )}

            <ItemLabel
              key={`label-${props.label}`}
              testId={`navigator-item-label-${props.label}`}
              name={props.label}
              isDynamic={props.isDynamic}
              target={props.navigatorEntry}
              selected={props.selected}
              dispatch={props.dispatch}
              inputVisible={EP.pathsEqual(props.renamingTarget, props.navigatorEntry.elementPath)}
            />
          </React.Fragment>,
        )}
        <ComponentPreview
          key={`preview-${props.label}`}
          navigatorEntry={props.navigatorEntry}
          color={props.iconColor}
        />
      </div>
    </React.Fragment>
  )
})

function elementContainsExpressions(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
): boolean {
  return MetadataUtils.isGeneratedTextFromMetadata(path, pathTrees, metadata)
}
