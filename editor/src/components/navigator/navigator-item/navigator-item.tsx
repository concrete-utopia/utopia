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
import type { ElementPath, HighlightBoundsWithFile } from '../../../core/shared/project-file-types'
import { unless } from '../../../utils/react-conditionals'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import type { IcnColor, IcnProps } from '../../../uuiui'
import { FlexRow, useColorTheme, UtopiaTheme } from '../../../uuiui'
import type { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { isEntryAPlaceholder } from '../../canvas/canvas-utils'
import type { EditorAction, EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as MetaActions from '../../editor/actions/meta-actions'
import type {
  ElementWarnings,
  NavigatorEntry,
  SlotNavigatorEntry,
} from '../../editor/store/editor-state'
import {
  defaultElementWarnings,
  isConditionalClauseNavigatorEntry,
  isInvalidOverrideNavigatorEntry,
  isRegularNavigatorEntry,
  isRenderPropNavigatorEntry,
  isSlotNavigatorEntry,
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
import { MapCounter } from './map-counter'
import { useCreateCallbackToShowComponentPicker } from './component-picker-context-menu'
import { getHighlightBoundsForProject } from '../../../core/model/project-file-utils'
import {
  selectedElementChangedMessageFromHighlightBounds,
  sendMessage,
} from '../../../core/vscode/vscode-bridge'
import { toVSCodeExtensionMessage } from 'utopia-vscode-common'
import type { Emphasis } from 'utopia-api'
import { contextMenu } from 'react-contexify'
import { DataReferenceCartoucheControl } from '../../inspector/sections/component-section/data-reference-cartouche'

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

export const BasePaddingUnit = 15

export function getElementPadding(withNavigatorDepth: number): number {
  const paddingOffset = withNavigatorDepth - 1
  return paddingOffset * BasePaddingUnit
}

export type ParentOutline = 'solid' | 'child' | 'none'

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
  highlightBounds: HighlightBoundsWithFile | null,
) {
  const elementPath = navigatorEntry.elementPath

  const shouldSelect = !(
    isConditionalClauseNavigatorEntry(navigatorEntry) ||
    isInvalidOverrideNavigatorEntry(navigatorEntry) ||
    isRenderPropNavigatorEntry(navigatorEntry) ||
    isSlotNavigatorEntry(navigatorEntry)
  )

  const selectionActions = shouldSelect
    ? getSelectionActions(getSelectedViewsInRange, index, elementPath, selected, event)
    : []

  // when we click on an already selected item we should force vscode to navigate there
  if (selected && shouldSelect && highlightBounds != null) {
    sendMessage(
      toVSCodeExtensionMessage(
        selectedElementChangedMessageFromHighlightBounds(highlightBounds, 'force-navigation'),
      ),
    )
  }

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

type StyleType =
  | 'default'
  | 'dynamic'
  | 'component'
  | 'componentInstance'
  | 'erroredGroup'
  | 'lowEmphasis'
  | 'highEmphasis'
type SelectedType = 'unselected' | 'selected' | 'descendantOfSelected'

const styleTypeColors: Record<StyleType, { color: keyof ThemeObject; iconColor: IcnColor }> = {
  default: { color: 'fg0', iconColor: 'main' },
  dynamic: { color: 'dynamicBlue', iconColor: 'dynamic' },
  component: { color: 'componentPurple', iconColor: 'component' },
  componentInstance: { color: 'fg0', iconColor: 'main' },
  erroredGroup: { color: 'error', iconColor: 'error' },
  lowEmphasis: { color: 'fg5', iconColor: 'darkgray' },
  highEmphasis: { color: 'dynamicBlue', iconColor: 'primary' },
}

const selectedTypeBackground: Record<SelectedType, keyof ThemeObject> = {
  unselected: 'bg1',
  selected: 'denimBlue',
  descendantOfSelected: 'lightDenimBlue',
}

const getColors = (
  styleType: StyleType,
  selectedType: SelectedType,
  colorTheme: ThemeObject,
): ComputedLook => {
  const { color: colorKey, iconColor } = styleTypeColors[styleType]
  const color = colorTheme[colorKey].value
  const background = colorTheme[selectedTypeBackground[selectedType]].value

  return {
    style: { background, color },
    iconColor: iconColor,
  }
}

const computeResultingStyle = (
  selected: boolean,
  emphasis: Emphasis,
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
  let styleType: StyleType = 'default'
  let selectedType: SelectedType = 'unselected'

  if (isErroredGroup) {
    styleType = 'erroredGroup'
  } else if (isDynamic) {
    styleType = 'dynamic'
  } else if (emphasis === 'subdued') {
    styleType = 'lowEmphasis'
  } else if (emphasis === 'emphasized') {
    styleType = 'highEmphasis'
  } else if (isInsideComponent || isFocusedComponent) {
    styleType = 'component'
  } else if (isFocusableComponent) {
    styleType = 'componentInstance'
  } else if (isHighlightedForInteraction) {
    styleType = 'default'
  }

  if (selected) {
    selectedType = 'selected'
  } else if (isDescendantOfSelected) {
    selectedType = 'descendantOfSelected'
  }

  let result = getColors(styleType, selectedType, colorTheme)

  const isProbablyParentOfSelected = (isProbablyScene || fullyVisible) && !selected

  result.style = {
    ...result.style,
    fontWeight: isProbablyParentOfSelected || isProbablyScene ? 600 : 'inherit',
    // TODO compute better borderRadius style by if it has children or siblings

    borderRadius: selected ? '5px 5px 0 0' : undefined,
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

        const isInsideFocusedComponent = EP.isInExplicitlyFocusedSubtree(
          store.editor.focusedElementPath,
          autoFocusedPaths,
          navigatorEntry.elementPath,
        )

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

type CodeItemType = 'conditional' | 'map' | 'code' | 'none' | 'remix'
export type RemixItemType = 'scene' | 'outlet' | 'link' | 'none'

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
  isOutletOrDescendantOfOutlet: boolean
  visibleNavigatorTargets: Array<NavigatorEntry>
}

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
    isOutletOrDescendantOfOutlet,
    getSelectedViewsInRange,
    index,
  } = props

  const highlightBounds = useEditorState(
    Substores.projectContents,
    (store) => {
      const staticPath = EP.dynamicPathToStaticPath(navigatorEntry.elementPath)
      if (staticPath != null) {
        const bounds = getHighlightBoundsForProject(store.editor.projectContents)
        if (bounds != null) {
          const highlightedUID = EP.toUid(staticPath)
          return bounds[highlightedUID]
        }
      }

      return null
    },
    'NavigatorItem highlightBounds',
  )

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

  const isInFocusedComponentSubtree = useEditorState(
    Substores.focusedElement,
    (store) =>
      isRegularNavigatorEntry(navigatorEntry) &&
      EP.isInExplicitlyFocusedSubtree(
        store.editor.focusedElementPath,
        autoFocusedPaths,
        navigatorEntry.elementPath,
      ),
    'NavigatorItem isInFocusedComponentSubtree',
  )

  const elementWarnings = useEditorState(
    Substores.derived,
    (store) => elementWarningsSelector(store, props.navigatorEntry),
    'NavigatorItem elementWarningsSelector',
  )

  const filePathMappings = useEditorState(
    Substores.derived,
    (store) => store.derived.filePathMappings,
    'NavigatorItem filePathMappings',
  )

  const isManuallyFocusableComponent = useEditorState(
    Substores.fullStore,
    (store) => {
      return (
        isRegularNavigatorEntry(navigatorEntry) &&
        MetadataUtils.isManuallyFocusableComponent(
          navigatorEntry.elementPath,
          store.editor.jsxMetadata,
          autoFocusedPaths,
          filePathMappings,
          store.editor.propertyControlsInfo,
          store.editor.projectContents,
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

  const codeItemType: CodeItemType = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isRegularNavigatorEntry(props.navigatorEntry)) {
        return 'none'
      }
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.navigatorEntry.elementPath,
      )
      if (MetadataUtils.isConditionalFromMetadata(elementMetadata)) {
        return 'conditional'
      }
      if (MetadataUtils.isJSXMapExpressionFromMetadata(elementMetadata)) {
        return 'map'
      }
      if (MetadataUtils.isExpressionOtherJavascriptFromMetadata(elementMetadata)) {
        return 'code'
      }
      if (
        MetadataUtils.isProbablyRemixOutletFromMetadata(elementMetadata) ||
        MetadataUtils.isProbablyRemixSceneFromMetadata(elementMetadata) ||
        MetadataUtils.isProbablyRemixLinkFromMetadata(elementMetadata)
      ) {
        return 'remix'
      }
      return 'none'
    },
    'NavigatorItem codeItemType',
  )

  const remixItemType: RemixItemType = useEditorState(
    Substores.metadata,
    (store) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.navigatorEntry.elementPath,
      )
      if (MetadataUtils.isProbablyRemixSceneFromMetadata(elementMetadata)) {
        return 'scene'
      }
      if (MetadataUtils.isProbablyRemixOutletFromMetadata(elementMetadata)) {
        return 'outlet'
      }
      if (MetadataUtils.isProbablyRemixLinkFromMetadata(elementMetadata)) {
        return 'link'
      }
      return 'none'
    },
    'NavigatorItem remixItemType',
  )

  const isConditional = codeItemType === 'conditional'
  const isRemixItem = codeItemType === 'remix'
  const isCodeItem = codeItemType !== 'none'

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

  const metadata = useEditorState(
    Substores.metadata,
    (store): ElementInstanceMetadataMap => {
      return store.editor.jsxMetadata
    },
    'NavigatorItem metadata',
  )

  const emphasis = useEditorState(
    Substores.propertyControlsInfo,
    (store): Emphasis => {
      return MetadataUtils.getEmphasisOfComponent(
        navigatorEntry.elementPath,
        metadata,
        store.editor.propertyControlsInfo,
        store.editor.projectContents,
      )
    },
    'NavigatorItem emphasis',
  )

  const isInsideComponent = isInFocusedComponentSubtree
  const fullyVisible = useStyleFullyVisible(navigatorEntry, autoFocusedPaths)
  const isProbablyScene = useIsProbablyScene(navigatorEntry)

  const elementIsData = navigatorEntry.type === 'DATA_REFERENCE'

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
    emphasis,
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
        highlightBounds,
      ),
    [
      dispatch,
      getSelectedViewsInRange,
      navigatorEntry,
      index,
      selected,
      conditionalOverrideUpdate,
      highlightBounds,
    ],
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

  const isPlaceholder = isEntryAPlaceholder(props.navigatorEntry)

  const isComponentScene = useIsProbablyScene(navigatorEntry) && childComponentCount === 1
  const isRenderProp = isRenderPropNavigatorEntry(navigatorEntry)

  const containerStyle: React.CSSProperties = React.useMemo(() => {
    return {
      opacity: isElementVisible && (!isHiddenConditionalBranch || isPlaceholder) ? undefined : 0.4,
      overflow: 'hidden',
      flexGrow: 1,
      flexShrink: 0,
    }
  }, [isElementVisible, isHiddenConditionalBranch, isPlaceholder])

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

  const iconColor = isRemixItem
    ? 'remix'
    : isDynamic
    ? 'dynamic'
    : emphasis === 'subdued'
    ? 'subdued'
    : emphasis === 'emphasized'
    ? 'primary'
    : isCodeItem
    ? 'dynamic'
    : isComponentScene
    ? 'component'
    : resultingStyle.iconColor

  const currentlyRenaming = EP.pathsEqual(props.renamingTarget, props.navigatorEntry.elementPath)
  const hideContextMenu = React.useCallback(() => contextMenu.hideAll(), [])

  return (
    <div
      onClick={hideContextMenu}
      style={{
        outline: `1px solid ${
          props.parentOutline === 'solid' && isOutletOrDescendantOfOutlet
            ? colorTheme.aqua.value
            : props.parentOutline === 'solid'
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
        {isPlaceholder ? (
          navigatorEntry.type === 'SLOT' ? (
            <RenderPropSlot
              label={props.label}
              parentOutline={props.parentOutline}
              navigatorEntry={navigatorEntry}
            />
          ) : (
            <PlaceholderSlot label={props.label} parentOutline={props.parentOutline} />
          )
        ) : isRenderProp ? (
          <div
            key={`label-${props.label}-slot`}
            style={{
              width: 140,
              height: 19,
              fontWeight: 600,
              textTransform: 'uppercase',
              color: colorTheme.fg5.value,
              border: colorTheme.navigatorResizeHintBorder.value,
              marginLeft: 23,
              paddingTop: 6,
              overflow: 'hidden',
            }}
          >
            {props.label}
          </div>
        ) : elementIsData ? (
          <div
            key={`label-${props.label}-slot`}
            style={{
              maxWidth: 140,
              color: colorTheme.fg5.value,
              border: colorTheme.navigatorResizeHintBorder.value,
              marginLeft: 23,
              paddingTop: 6,
              overflow: 'hidden',
            }}
          >
            <DataReferenceCartoucheControl
              elementPath={navigatorEntry.elementPath}
              childOrAttribute={navigatorEntry.childOrAttribute}
            />
          </div>
        ) : (
          <FlexRow
            style={{
              justifyContent: 'space-between',
              ...containerStyle,
              padding: '0 5px',
              width: '100%',
            }}
          >
            <FlexRow style={{ width: '100%' }}>
              {unless(
                props.navigatorEntry.type === 'CONDITIONAL_CLAUSE',
                <ExpandableIndicator
                  key='expandable-indicator'
                  visible={showExpandableIndicator}
                  collapsed={collapsed}
                  selected={selected && !isInsideComponent}
                  onMouseDown={collapse}
                  style={{
                    opacity: 'var(--paneHoverOpacity)',
                  }}
                  testId={`navigator-item-collapse-${navigatorEntryToKey(props.navigatorEntry)}`}
                  iconColor={iconColor}
                />,
              )}
              <NavigatorRowLabel
                shouldShowParentOutline={props.parentOutline === 'child'}
                navigatorEntry={navigatorEntry}
                label={props.label}
                renamingTarget={props.renamingTarget}
                selected={props.selected}
                codeItemType={codeItemType}
                remixItemType={remixItemType}
                emphasis={emphasis}
                dispatch={props.dispatch}
                isDynamic={isDynamic}
                iconColor={iconColor}
                elementWarnings={!isConditional ? elementWarnings : null}
                childComponentCount={childComponentCount}
              />
            </FlexRow>
            {unless(
              currentlyRenaming || props.navigatorEntry.type === 'CONDITIONAL_CLAUSE',
              <NavigatorItemActionSheet
                navigatorEntry={navigatorEntry}
                selected={selected}
                highlighted={isHighlighted}
                isVisibleOnCanvas={isElementVisible}
                instanceOriginalComponentName={null}
                dispatch={dispatch}
                isSlot={isPlaceholder}
                iconColor={iconColor}
                background={rowStyle.background}
              />,
            )}
          </FlexRow>
        )}
      </FlexRow>
    </div>
  )
})
NavigatorItem.displayName = 'NavigatorItem'

interface RenderPropSlotProps {
  label: string
  parentOutline: ParentOutline
  navigatorEntry: SlotNavigatorEntry
}

const RenderPropSlot = React.memo((props: RenderPropSlotProps) => {
  const { label, parentOutline, navigatorEntry } = props
  const target = EP.parentPath(navigatorEntry.elementPath)
  const insertionTarget = { prop: navigatorEntry.prop }

  const showComponentPickerContextMenu = useCreateCallbackToShowComponentPicker()(
    target,
    insertionTarget,
  )

  return (
    <PlaceholderSlot
      label={label}
      parentOutline={parentOutline}
      cursor={'pointer'}
      onClick={showComponentPickerContextMenu}
      testId={`toggle-render-prop-${NavigatorItemTestId(
        varSafeNavigatorEntryToKey(navigatorEntry),
      )}`}
    />
  )
})

interface PlaceholderSlotProps {
  label: string
  parentOutline: ParentOutline
  cursor?: 'pointer' | 'inherit'
  testId?: string
  onClick?: React.MouseEventHandler
}

const PlaceholderSlot = React.memo((props: PlaceholderSlotProps) => {
  const { label, parentOutline, cursor, testId, onClick } = props
  const colorTheme = useColorTheme()

  return (
    <div
      key={`label-${label}-slot`}
      onClick={onClick}
      style={{
        width: 140,
        height: 19,
        borderRadius: 20,
        textAlign: 'center',
        textTransform: 'lowercase',
        backgroundColor: colorTheme.unavailable.value,
        color:
          parentOutline === 'child'
            ? colorTheme.navigatorResizeHintBorder.value
            : colorTheme.unavailableGrey10.value,
        border: `1px solid ${
          parentOutline === 'child'
            ? colorTheme.navigatorResizeHintBorder.value
            : colorTheme.unavailableGrey10.value
        }`,
        marginLeft: 28,
        cursor: cursor ?? 'inherit',
      }}
      data-testid={testId}
    >
      Empty
    </div>
  )
})
interface NavigatorRowLabelProps {
  navigatorEntry: NavigatorEntry
  iconColor: IcnProps['color']
  elementWarnings: ElementWarnings | null
  label: string
  isDynamic: boolean
  renamingTarget: ElementPath | null
  selected: boolean
  codeItemType: CodeItemType
  remixItemType: RemixItemType
  emphasis: Emphasis
  shouldShowParentOutline: boolean
  childComponentCount: number
  dispatch: EditorDispatch
}

export const NavigatorRowLabel = React.memo((props: NavigatorRowLabelProps) => {
  const colorTheme = useColorTheme()

  const isCodeItem = props.codeItemType !== 'none' && props.codeItemType !== 'remix'
  const isRemixItem = props.codeItemType === 'remix'
  const isComponentScene =
    useIsProbablyScene(props.navigatorEntry) && props.childComponentCount === 1

  const textColor = isRemixItem
    ? colorTheme.aqua.value
    : props.isDynamic
    ? colorTheme.dynamicBlue.value
    : props.emphasis === 'subdued'
    ? colorTheme.fg5.value
    : props.emphasis === 'emphasized'
    ? colorTheme.dynamicBlue.value
    : isCodeItem
    ? colorTheme.dynamicBlue.value
    : isComponentScene
    ? colorTheme.componentPurple.value
    : undefined

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        justifyContent: 'flex-start',
        gap: 5,
        borderRadius: 20,
        height: 22,
        paddingLeft: 5,
        paddingRight: props.codeItemType === 'map' ? 0 : 5,
        color: textColor,
        textTransform: isCodeItem ? 'uppercase' : undefined,
      }}
    >
      {unless(
        props.navigatorEntry.type === 'CONDITIONAL_CLAUSE' ||
          props.navigatorEntry.type === 'RENDER_PROP',
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
        remixItemType={props.remixItemType}
      />
      <MapCounter navigatorEntry={props.navigatorEntry} dispatch={props.dispatch} />
      <ComponentPreview
        key={`preview-${props.label}`}
        navigatorEntry={props.navigatorEntry}
        color={props.iconColor}
      />
    </div>
  )
})

function elementContainsExpressions(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
): boolean {
  return MetadataUtils.isGeneratedTextFromMetadata(path, pathTrees, metadata)
}
