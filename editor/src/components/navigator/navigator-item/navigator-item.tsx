/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import type { CSSProperties } from 'react'
import React from 'react'
import type { ConditionalCase } from '../../../core/model/conditionals'
import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  getConditionalCaseCorrespondingToBranchPath,
  getConditionalClausePath,
  getConditionalFlag,
  maybeConditionalActiveBranch,
  maybeConditionalExpression,
  useConditionalCaseCorrespondingToBranchPath,
} from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { hasElementsWithin } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { unless } from '../../../utils/react-conditionals'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import type { IcnColor, IcnProps } from '../../../uuiui'
import { FlexRow, useColorTheme, UtopiaTheme } from '../../../uuiui'
import type { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { isEntryAPlaceholder } from '../../canvas/canvas-utils'
import type { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import type {
  ElementWarnings,
  NavigatorEntry,
  SlotNavigatorEntry,
} from '../../editor/store/editor-state'
import {
  defaultElementWarnings,
  isConditionalClauseNavigatorEntry,
  isDataReferenceNavigatorEntry,
  isRegularNavigatorEntry,
  isRenderPropNavigatorEntry,
  isSyntheticNavigatorEntry,
  navigatorEntryToKey,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type {
  DerivedSubstate,
  MetadataSubstate,
} from '../../editor/store/store-hook-substore-types'
import { ComponentPreview } from './component-preview'
import { ExpandableIndicator } from './expandable-indicator'
import { ItemLabel } from './item-label'
import { LayoutIcon } from './layout-icon'
import { NavigatorItemActionSheet } from './navigator-item-components'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import {
  conditionalTarget,
  renderPropTarget,
  useCreateCallbackToShowComponentPicker,
} from './component-picker-context-menu'
import type { Emphasis, Icon } from 'utopia-api'
import { contextMenu } from 'react-contexify'
import { DataReferenceCartoucheControl } from '../../inspector/sections/component-section/data-reference-cartouche'
import { MapListSourceCartoucheNavigator } from '../../inspector/sections/layout-section/list-source-cartouche'
import { regularNavigatorRow } from '../navigator-row'
import { NavigatorRowClickableWrapper } from './navigator-item-clickable-wrapper'
import { useNavigatorSelectionBoundsForEntry } from './use-navigator-selection-bounds-for-entry'

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

export const BasePaddingUnit = 17

export function getElementPadding(withNavigatorDepth: number): number {
  const paddingOffset = withNavigatorDepth
  return paddingOffset * BasePaddingUnit
}

export type ParentOutline = 'solid' | 'child' | 'none'

const highlightItem = (
  dispatch: EditorDispatch,
  navigatorEntry: NavigatorEntry,
  selected: boolean,
  highlighted: boolean,
) => {
  const elementPath = getSelectionTargetForNavigatorEntry(navigatorEntry)
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
  e.stopPropagation()
  dispatch([EditorActions.toggleCollapse(elementPath)], 'leftpane')
}

type StyleType =
  | 'default'
  | 'dynamic'
  | 'component'
  | 'componentInstance'
  | 'erroredGroup'
  | 'lowEmphasis'
  | 'selected'
type SelectedType =
  | 'unselected'
  | 'selected'
  | 'descendantOfSelected'
  | 'selectedFocusedComponent'
  | 'descendantOfSelectedFocusedComponent'

const styleTypeColors: Record<StyleType, { color: keyof ThemeObject; iconColor: IcnColor }> = {
  default: { color: 'fg0', iconColor: 'main' },
  dynamic: { color: 'dynamicBlue', iconColor: 'dynamic' },
  component: { color: 'componentPurple', iconColor: 'component' },
  componentInstance: { color: 'fg0', iconColor: 'main' },
  erroredGroup: { color: 'error', iconColor: 'error' },
  lowEmphasis: { color: 'fg5', iconColor: 'subdued' },
  selected: { color: 'white', iconColor: 'white' },
}

const selectedTypeBackground: Record<SelectedType, keyof ThemeObject> = {
  unselected: 'bg1',
  selected: 'selectionBlue',
  descendantOfSelected: 'childSelectionBlue',
  selectedFocusedComponent: 'selectionPurple',
  descendantOfSelectedFocusedComponent: 'childSelectionPurple',
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

export const NavigatorRowBorderRadius = 5

const computeResultingStyle = (params: {
  selected: boolean
  isTopOfSelection: boolean
  isBottomOfSelection: boolean
  emphasis: Emphasis
  isInsideComponent: boolean
  isProbablyScene: boolean
  fullyVisible: boolean
  isFocusedComponent: boolean
  isFocusableComponent: boolean
  isHighlightedForInteraction: boolean
  isDescendantOfSelected: boolean
  isErroredGroup: boolean
  colorTheme: ThemeObject
}) => {
  const {
    selected,
    isTopOfSelection,
    isBottomOfSelection,
    emphasis,
    isInsideComponent,
    isProbablyScene,
    fullyVisible,
    isFocusedComponent,
    isFocusableComponent,
    isHighlightedForInteraction,
    isDescendantOfSelected,
    isErroredGroup,
    colorTheme,
  } = params
  let styleType: StyleType = 'default'
  let selectedType: SelectedType = 'unselected'

  if (selected) {
    styleType = 'selected'
  } else if (isErroredGroup) {
    styleType = 'erroredGroup'
  } else if (
    (!isProbablyScene && isFocusedComponent) ||
    (isDescendantOfSelected && isInsideComponent)
  ) {
    styleType = 'component'
  } else if (emphasis === 'subdued') {
    styleType = 'lowEmphasis'
  } else if (isFocusableComponent) {
    styleType = 'componentInstance'
  } else if (isHighlightedForInteraction) {
    styleType = 'default'
  }
  const isProbablyParentOfSelected = (isProbablyScene || fullyVisible) && !selected

  if (
    selected &&
    (isFocusedComponent || isInsideComponent) &&
    !isProbablyScene &&
    !isProbablyParentOfSelected
  ) {
    selectedType = 'selectedFocusedComponent'
  } else if (selected) {
    selectedType = 'selected'
  } else if (isDescendantOfSelected && isInsideComponent) {
    selectedType = 'descendantOfSelectedFocusedComponent'
  } else if (isDescendantOfSelected) {
    selectedType = 'descendantOfSelected'
  } else {
    selectedType = 'unselected'
  }

  let result = getColors(styleType, selectedType, colorTheme)

  const borderRadiusTop = isTopOfSelection ? NavigatorRowBorderRadius : 0
  const borderRadiusBottom = isBottomOfSelection ? NavigatorRowBorderRadius : 0

  result.style = {
    ...result.style,
    fontWeight: isProbablyParentOfSelected || isProbablyScene ? 600 : 'inherit',
    borderTopLeftRadius: borderRadiusTop,
    borderTopRightRadius: borderRadiusTop,
    borderBottomLeftRadius: borderRadiusBottom,
    borderBottomRightRadius: borderRadiusBottom,
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

export const elementWarningsSelector = createCachedSelector(
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
  indentation: number
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
  const { dispatch, isHighlighted, isElementVisible, selected, collapsed, navigatorEntry } = props

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

  const entryNavigatorDepth = props.indentation

  const containsExpressions: boolean = useEditorState(
    Substores.metadata,
    (store) => {
      return elementContainsExpressions(
        navigatorEntry.elementPath,
        store.editor.jsxMetadata,
        store.editor.elementPathTree,
      )
    },
    'NavigatorItem containsExpressions',
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

  const isGenerated = MetadataUtils.isElementOrAncestorGenerated(navigatorEntry.elementPath)
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

  const iconOverride = useEditorState(
    Substores.propertyControlsInfo,
    (store) =>
      MetadataUtils.getIconOfComponent(
        navigatorEntry.elementPath,
        store.editor.propertyControlsInfo,
        store.editor.projectContents,
      ),
    'NavigatorItem iconOverride',
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

  const toggleCollapse = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
      collapseItem(dispatch, navigatorEntry.elementPath, event)
      event.stopPropagation()
    },
    [dispatch, navigatorEntry.elementPath],
  )

  const highlight = React.useCallback(
    () => highlightItem(dispatch, navigatorEntry, selected, isHighlighted),
    [dispatch, navigatorEntry, selected, isHighlighted],
  )

  const focusComponent = React.useCallback(
    (event: React.MouseEvent) => {
      if (isManuallyFocusableComponent && !event.altKey) {
        const elementPath = getSelectionTargetForNavigatorEntry(navigatorEntry)
        dispatch([EditorActions.setFocusedElement(elementPath)])
      }
    },
    [dispatch, navigatorEntry, isManuallyFocusableComponent],
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

  const conditionalCase = useConditionalCaseCorrespondingToBranchPath(
    props.navigatorEntry.elementPath,
  )

  const isPlaceholder = isEntryAPlaceholder(props.navigatorEntry)

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

  const canBeExpanded = React.useMemo(() => {
    return (
      isConditional || // if it is a conditional, so it could have no children if both branches are null
      childComponentCount > 0 ||
      isFocusedComponent
    )
  }, [childComponentCount, isFocusedComponent, isConditional])

  const { isTopOfSelection, isBottomOfSelection } = useNavigatorSelectionBoundsForEntry(
    navigatorEntry,
    selected,
    childComponentCount,
  )

  const resultingStyle = computeResultingStyle({
    selected: elementIsData ? false : selected,
    isTopOfSelection: isTopOfSelection,
    isBottomOfSelection: isBottomOfSelection,
    emphasis: emphasis,
    isInsideComponent: isInsideComponent,
    isProbablyScene: isProbablyScene,
    fullyVisible: fullyVisible,
    isFocusedComponent: isFocusedComponent,
    isFocusableComponent: isManuallyFocusableComponent,
    isHighlightedForInteraction: isHighlightedForInteraction,
    isDescendantOfSelected: elementIsData && selected ? false : isDescendantOfSelected,
    isErroredGroup: isErroredGroup,
    colorTheme: colorTheme,
  })

  const rowStyle = useKeepReferenceEqualityIfPossible({
    paddingLeft: getElementPadding(entryNavigatorDepth),
    height: getItemHeight(navigatorEntry),
    ...resultingStyle.style,
    ...cursorStyle,
  })

  const iconColor = resultingStyle.iconColor

  const currentlyRenaming = EP.pathsEqual(props.renamingTarget, props.navigatorEntry.elementPath)

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent) => {
      // Only do this for a left mouse down.
      if (e.button === 0) {
        if (isRenderPropNavigatorEntry(navigatorEntry)) {
          e.stopPropagation()
        }

        contextMenu.hideAll()
      }
    },
    [navigatorEntry],
  )

  const isScene = React.useMemo(() => {
    return (
      MetadataUtils.isProbablyScene(metadata, props.navigatorEntry.elementPath) ||
      MetadataUtils.isProbablyRemixScene(metadata, props.navigatorEntry.elementPath)
    )
  }, [props.navigatorEntry, metadata])

  const hideExpandableIndicator = React.useMemo(() => {
    return props.navigatorEntry.type === 'CONDITIONAL_CLAUSE' || isScene
  }, [props.navigatorEntry, isScene])

  return (
    <NavigatorRowClickableWrapper
      row={regularNavigatorRow(props.navigatorEntry, props.indentation)}
    >
      <div
        onMouseDown={onMouseDown}
        style={{
          flex: 1,
          borderRadius: 5,
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
            ) : conditionalCase !== null ? (
              <ConditionalBranchSlot
                label={props.label}
                parentOutline={props.parentOutline}
                navigatorEntry={navigatorEntry}
                conditionalCase={conditionalCase}
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
              key={`data-reference-${props.label}`}
              style={{
                maxWidth: 140,
                color: colorTheme.fg5.value,
                border: colorTheme.navigatorResizeHintBorder.value,
                marginLeft: 23,
                overflow: 'hidden',
              }}
            >
              <DataReferenceCartoucheControl
                elementPath={navigatorEntry.elementPath}
                renderedAt={navigatorEntry.renderedAt}
                surroundingScope={navigatorEntry.surroundingScope}
                childOrAttribute={navigatorEntry.childOrAttribute}
                selected={selected}
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
                  hideExpandableIndicator,
                  <ExpandableIndicator
                    key='expandable-indicator'
                    visible={canBeExpanded}
                    collapsed={collapsed}
                    selected={selected && !isInsideComponent}
                    onMouseDown={toggleCollapse}
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
                  iconOverride={iconOverride}
                  elementWarnings={!isConditional ? elementWarnings : null}
                  childComponentCount={childComponentCount}
                  insideFocusedComponent={isInsideComponent && isDescendantOfSelected}
                  style={{
                    paddingLeft: isScene ? 0 : 5,
                    paddingRight: codeItemType === 'map' ? 0 : 5,
                  }}
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
                  isScene={isScene}
                  collapsed={collapsed}
                />,
              )}
            </FlexRow>
          )}
        </FlexRow>
      </div>
    </NavigatorRowClickableWrapper>
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
  const insertionTarget = renderPropTarget(navigatorEntry.prop)

  const showComponentPickerContextMenu = useCreateCallbackToShowComponentPicker()(
    [target],
    insertionTarget,
  ) as React.MouseEventHandler<Element>

  return (
    <PlaceholderSlot
      label={label}
      parentOutline={parentOutline}
      cursor={'pointer'}
      onMouseDown={showComponentPickerContextMenu}
      testId={`toggle-render-prop-${NavigatorItemTestId(
        varSafeNavigatorEntryToKey(navigatorEntry),
      )}`}
    />
  )
})

interface ConditionalBranchSlotProps {
  label: string
  parentOutline: ParentOutline
  navigatorEntry: NavigatorEntry
  conditionalCase: ConditionalCase
}

const ConditionalBranchSlot = React.memo((props: ConditionalBranchSlotProps) => {
  const { label, parentOutline, navigatorEntry, conditionalCase } = props
  const target = EP.parentPath(navigatorEntry.elementPath)

  const showComponentPickerContextMenu = useCreateCallbackToShowComponentPicker()(
    [target],
    conditionalTarget(conditionalCase),
  ) as React.MouseEventHandler<Element>

  return (
    <PlaceholderSlot
      label={label}
      parentOutline={parentOutline}
      cursor={'pointer'}
      onMouseDown={showComponentPickerContextMenu}
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
  onMouseDown?: React.MouseEventHandler
}

const PlaceholderSlot = React.memo((props: PlaceholderSlotProps) => {
  const { label, parentOutline, cursor, testId, onMouseDown } = props
  const colorTheme = useColorTheme()

  return (
    <div
      key={`label-${label}-slot`}
      onMouseDown={onMouseDown}
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
  iconOverride: Icon | null
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
  insideFocusedComponent: boolean
  style?: CSSProperties
}

export const NavigatorRowLabel = React.memo((props: NavigatorRowLabelProps) => {
  const isCodeItem = props.codeItemType !== 'none' && props.codeItemType !== 'remix'

  return (
    <div
      style={{
        ...props.style,
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        justifyContent: 'flex-start',
        gap: 5,
        borderRadius: 20,
        height: 22,
      }}
    >
      {unless(
        props.navigatorEntry.type === 'CONDITIONAL_CLAUSE' ||
          props.navigatorEntry.type === 'RENDER_PROP',
        <LayoutIcon
          key={`layout-type-${props.label}`}
          navigatorEntry={props.navigatorEntry}
          override={props.iconOverride}
          color={
            props.selected
              ? 'white'
              : props.insideFocusedComponent
              ? 'component'
              : props.emphasis === 'subdued'
              ? 'subdued'
              : props.iconColor
          }
          elementWarnings={props.elementWarnings}
        />,
      )}

      <div
        style={{
          textTransform: isCodeItem ? 'uppercase' : undefined,
          display: props.codeItemType === 'map' ? 'none' : 'block',
        }}
      >
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
      </div>
      <MapListSourceCartoucheNavigator
        target={props.navigatorEntry.elementPath}
        selected={props.selected}
        openOn='double-click'
      />
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

function getSelectionTargetForNavigatorEntry(navigatorEntry: NavigatorEntry): ElementPath {
  if (isDataReferenceNavigatorEntry(navigatorEntry)) {
    return EP.parentPath(navigatorEntry.elementPath)
  }
  if (isRenderPropNavigatorEntry(navigatorEntry) && navigatorEntry.childPath != null) {
    return navigatorEntry.childPath
  }

  return navigatorEntry.elementPath
}
