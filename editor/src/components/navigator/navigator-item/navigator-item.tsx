/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as MetaActions from '../../editor/actions/meta-actions'
import * as EP from '../../../core/shared/element-path'
import { ExpandableIndicator } from './expandable-indicator'
import { ItemLabel } from './item-label'
import { ComponentPreview } from './component-preview'
import { NavigatorItemActionSheet } from './navigator-item-components'
import {
  defaultElementWarnings,
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  NavigatorEntry,
  navigatorEntryToKey,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import { ChildWithPercentageSize } from '../../common/size-warnings'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { IcnProps, useColorTheme, UtopiaStyles, UtopiaTheme, FlexRow } from '../../../uuiui'
import { LayoutIcon } from './layout-icon'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { unless, when } from '../../../utils/react-conditionals'
import { isLeft, isRight } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isNullJSXAttributeValue,
  JSXConditionalExpression,
} from '../../../core/shared/element-template'
import {
  getConditionalClausePath,
  getConditionalFlag,
  matchesOverriddenConditionalBranch,
} from '../../../core/model/conditionals'
import { DerivedSubstate, MetadataSubstate } from '../../editor/store/store-hook-substore-types'
import { getConditionalClausePathForNavigatorEntry, navigatorDepth } from '../navigator-utils'
import createCachedSelector from 're-reselect'
import { getValueFromComplexMap } from '../../../utils/map'
import { isSyntheticNavigatorEntry } from '../../editor/store/editor-state'
import { getElementContentAffectingType } from '../../canvas/canvas-strategies/strategies/group-like-helpers'

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

function selectItem(
  dispatch: EditorDispatch,
  getSelectedViewsInRange: (i: number) => Array<ElementPath>,
  navigatorEntry: NavigatorEntry,
  index: number,
  selected: boolean,
  event: React.MouseEvent<HTMLDivElement>,
  elementMetadata: ElementInstanceMetadata | null,
) {
  const elementPath =
    isConditionalClauseNavigatorEntry(navigatorEntry) && elementMetadata != null
      ? getConditionalClausePathForNavigatorEntry(navigatorEntry, elementMetadata)
      : navigatorEntry.elementPath

  if (elementPath == null) {
    return
  }

  if (!selected) {
    if (event.metaKey && !event.shiftKey) {
      // adds to selection
      dispatch(MetaActions.selectComponents([elementPath], true), 'leftpane')
    } else if (event.shiftKey) {
      // selects range of items
      const targets = getSelectedViewsInRange(index)
      dispatch(MetaActions.selectComponents(targets, false), 'leftpane')
    } else {
      dispatch(MetaActions.selectComponents([elementPath], false), 'leftpane')
    }
  }
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

const defaultUnselected = (colorTheme: any): ComputedLook => ({
  style: { background: 'transparent', color: colorTheme.fg0.value },
  iconColor: 'main',
})

const defaultSelected = (colorTheme: any): ComputedLook => ({
  style: { background: UtopiaStyles.backgrounds.blue, color: colorTheme.white.value },
  iconColor: 'on-highlight-main',
})

const dynamicUnselected = (colorTheme: any): ComputedLook => ({
  style: { background: 'transparent', color: colorTheme.primary.value },
  iconColor: 'primary',
})

const dynamicSelected = (colorTheme: any): ComputedLook => ({
  style: { background: UtopiaStyles.backgrounds.lightblue, color: colorTheme.white.value },
  iconColor: 'on-highlight-main',
})

const componentUnselected = (colorTheme: any): ComputedLook => ({
  style: {
    background: colorTheme.emphasizedBackground.value,
    color: colorTheme.neutralForeground.value,
  },
  iconColor: 'warning',
})

const componentSelected = (colorTheme: ThemeObject): ComputedLook => ({
  style: {
    background: colorTheme.navigatorComponentSelected.value,
    color: colorTheme.neutralForeground.value,
  },
  iconColor: 'warning',
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
  colorTheme: ThemeObject,
) => {
  let result = defaultUnselected(colorTheme)
  if (isHighlightedForInteraction) {
    result = {
      style: {
        background: colorTheme.brandPurple70.value,
        color: colorTheme.white.value,
      },
      iconColor: 'main',
    }
  } else if (isInsideComponent) {
    result = componentUnselected(colorTheme)
  } else if (isDynamic) {
    result = dynamicUnselected(colorTheme)
  } else {
    result = defaultUnselected(colorTheme)
  }

  if (selected) {
    if (isFocusableComponent && !isFocusedComponent) {
      result = {
        style: { backgroundColor: colorTheme.brandPurple.value, color: colorTheme.white.value },
        iconColor: 'on-highlight-main',
      }
    } else if (isInsideComponent) {
      result = componentSelected(colorTheme)
    } else if (isDynamic) {
      result = dynamicSelected(colorTheme)
    } else {
      result = defaultSelected(colorTheme)
    }
  }

  // additional style
  result.style = {
    ...result.style,
    fontWeight: isProbablyScene || fullyVisible ? 600 : 'inherit',
    '--iconOpacity': fullyVisible ? 1 : 0.4,
  }

  return result
}

function useStyleFullyVisible(navigatorEntry: NavigatorEntry): boolean {
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
          EP.isFocused(store.editor.focusedElementPath, path) || EP.isInsideFocusedComponent(path)

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
  (store: MetadataSubstate, _elementPath: ElementPath, parentPath: ElementPath) =>
    MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, parentPath),
  (_store: MetadataSubstate, elementPath: ElementPath, _parentPath: ElementPath) => elementPath,
  (_store: MetadataSubstate, _elementPath: ElementPath, parentPath: ElementPath) => parentPath,
  (
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

    const conditional = asConditional(parent)
    if (conditional == null) {
      return false
    }

    const flag = getConditionalFlag(conditional)

    // the final condition value, either from the original or from the override
    const overriddenConditionValue: boolean = flag ?? originalConditionValue

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

const isActiveBranchOfOverriddenConditionalSelector = createCachedSelector(
  (store: MetadataSubstate, _elementPath: ElementPath, parentPath: ElementPath) =>
    MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, parentPath),
  (_store: MetadataSubstate, elementPath: ElementPath, _parentPath: ElementPath) => elementPath,
  (_store: MetadataSubstate, _elementPath: ElementPath, parentPath: ElementPath) => parentPath,
  (parent: ElementInstanceMetadata | null, elementPath: ElementPath, parentPath: ElementPath) => {
    const conditionalParent = asConditional(parent)
    if (conditionalParent == null) {
      return false
    }
    const parentOverride = getConditionalFlag(conditionalParent)
    if (parentOverride == null) {
      return false
    }

    return (
      matchesOverriddenConditionalBranch(elementPath, parentPath, {
        clause: conditionalParent.whenTrue,
        wantOverride: true,
        parentOverride: parentOverride,
      }) ||
      matchesOverriddenConditionalBranch(elementPath, parentPath, {
        clause: conditionalParent.whenFalse,
        wantOverride: false,
        parentOverride: parentOverride,
      })
    )
  },
)((_, elementPath, parentPath) => `${EP.toString(elementPath)}_${EP.toString(parentPath)}`)

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
  const isFocusedComponent = useEditorState(
    Substores.focusedElement,
    (store) =>
      isRegularNavigatorEntry(navigatorEntry) &&
      EP.isFocused(store.editor.focusedElementPath, navigatorEntry.elementPath),
    'NavigatorItem isFocusedComponent',
  )

  const elementWarnings = useEditorState(
    Substores.derived,
    (store) => elementWarningsSelector(store, props.navigatorEntry),
    'NavigatorItem elementWarningsSelector',
  )

  const isFocusableComponent = useEditorState(
    Substores.metadata,
    (store) => {
      return (
        isRegularNavigatorEntry(navigatorEntry) &&
        MetadataUtils.isFocusableComponent(navigatorEntry.elementPath, store.editor.jsxMetadata)
      )
    },
    'NavigatorItem isFocusable',
  )

  const entryNavigatorDepth = useEditorState(
    Substores.metadata,
    (store) => {
      return navigatorDepth(navigatorEntry, store.editor.jsxMetadata)
    },
    'NavigatorItem entryNavigatorDepth',
  )

  const childComponentCount = props.noOfChildren

  const isDynamic = MetadataUtils.isElementGenerated(navigatorEntry.elementPath)
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

  const elementMetadata = useEditorState(
    Substores.metadata,
    (store) =>
      MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, navigatorEntry.elementPath),
    'NavigatorItem elementMetadata',
  )

  const isInsideComponent =
    EP.isInsideFocusedComponent(navigatorEntry.elementPath) || isFocusedComponent
  const fullyVisible = useStyleFullyVisible(navigatorEntry)
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

  const resultingStyle = computeResultingStyle(
    selected,
    isInsideComponent,
    isDynamic,
    isProbablyScene,
    fullyVisible,
    isFocusedComponent,
    isFocusableComponent,
    isHighlightedForInteraction,
    colorTheme,
  )

  let warningText: string | null = null
  if (!isConditional) {
    if (elementWarnings.dynamicSceneChildWidthHeightPercentage) {
      warningText = ChildWithPercentageSize
    } else if (elementWarnings.widthOrHeightZero) {
      warningText = 'Missing width or height'
    } else if (elementWarnings.absoluteWithUnpositionedParent) {
      warningText = 'Element is trying to be position absolutely with an unconfigured parent'
    }
  }

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
        elementMetadata,
      ),
    [dispatch, getSelectedViewsInRange, navigatorEntry, index, selected, elementMetadata],
  )
  const highlight = React.useCallback(
    () => highlightItem(dispatch, navigatorEntry.elementPath, selected, isHighlighted),
    [dispatch, navigatorEntry.elementPath, selected, isHighlighted],
  )
  const focusComponent = React.useCallback(
    (event: React.MouseEvent) => {
      if (isFocusableComponent && !event.altKey) {
        dispatch([EditorActions.setFocusedElement(navigatorEntry.elementPath)])
      }
    },
    [dispatch, navigatorEntry.elementPath, isFocusableComponent],
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

  const parentElement = useEditorState(
    Substores.metadata,
    (store) => {
      const parentPath = EP.parentPath(props.navigatorEntry.elementPath)
      return MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, parentPath)
    },
    'NavigatorItem parentElement',
  )

  const isSlot = React.useMemo(() => {
    const isParentConditional =
      parentElement != null &&
      isRight(parentElement.element) &&
      isJSXConditionalExpression(parentElement.element.value)

    const isNullValue =
      isSyntheticNavigatorEntry(props.navigatorEntry) &&
      isNullJSXAttributeValue(props.navigatorEntry.childOrAttribute)

    return isParentConditional && isNullValue
  }, [parentElement, props.navigatorEntry])

  const containerStyle: React.CSSProperties = React.useMemo(() => {
    return {
      opacity: isElementVisible && (!isHiddenConditionalBranch || isSlot) ? undefined : 0.4,
      overflow: 'hidden',
      flexGrow: 1,
      flexShrink: 0,
    }
  }, [isElementVisible, isHiddenConditionalBranch, isSlot])

  const rowStyle = useKeepReferenceEqualityIfPossible({
    paddingLeft: getElementPadding(entryNavigatorDepth),
    height: UtopiaTheme.layout.rowHeight.smaller,
    ...resultingStyle.style,
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
        border: `1px solid ${
          props.parentOutline === 'solid'
            ? colorTheme.navigatorResizeHintBorder.value
            : 'transparent'
        }`,
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
          />
          <NavigatorRowLabel
            shouldShowParentOutline={props.parentOutline === 'child'}
            navigatorEntry={navigatorEntry}
            label={props.label}
            renamingTarget={props.renamingTarget}
            selected={props.selected}
            dispatch={props.dispatch}
            isDynamic={isDynamic}
            iconColor={resultingStyle.iconColor}
            warningText={warningText}
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
        />
      </FlexRow>
    </div>
  )
})
NavigatorItem.displayName = 'NavigatorItem'

interface NavigatorRowLabelProps {
  navigatorEntry: NavigatorEntry
  iconColor: IcnProps['color']
  warningText: string | null
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

  const element = useEditorState(
    Substores.metadata,
    (store) => {
      return MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.navigatorEntry.elementPath,
      )
    },
    'NavigatorRowLabel element',
  )

  const conditionalOverride = React.useMemo(() => {
    const conditional = asConditional(element)
    if (conditional == null) {
      return null
    }
    return getConditionalFlag(conditional)
  }, [element])

  const isActiveBranchOfOverriddenConditional = useEditorState(
    Substores.metadata,
    (store) => {
      return (
        isConditionalClauseNavigatorEntry(props.navigatorEntry) &&
        isActiveBranchOfOverriddenConditionalSelector(
          store,
          props.navigatorEntry.elementPath,
          EP.parentPath(props.navigatorEntry.elementPath),
        )
      )
    },
    'NavigatorRowLabel isActiveBranchOfOverriddenConditional',
  )

  return (
    <React.Fragment>
      {when(
        props.isSlot,
        <div
          key={`label-${props.label}-slot`}
          style={{
            border: `1px solid ${
              props.selected
                ? colorTheme.bg0.value
                : props.shouldShowParentOutline
                ? colorTheme.navigatorResizeHintBorder.value
                : colorTheme.fg7.value
            }`,
            opacity: props.selected ? 0.8 : 1,
            width: '100%',
            padding: '2px 6px',
            borderRadius: 2,
            color: props.selected ? colorTheme.bg0.value : colorTheme.fg8.value,
            textTransform: 'lowercase',
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
              warningText={props.warningText}
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
            style={{
              color:
                !props.selected && isActiveBranchOfOverriddenConditional
                  ? colorTheme.brandNeonPink.value
                  : 'inherit',
            }}
          />

          {when(
            conditionalOverride != null && props.navigatorEntry.type !== 'CONDITIONAL_CLAUSE',
            <div
              style={{
                marginLeft: 10,
                color: colorTheme.bg0.value,
                background: colorTheme.brandNeonPink.value,
                borderRadius: 10,
                padding: '0px 6px',
                fontWeight: 600,
                textTransform: 'uppercase',
                fontSize: 9,
              }}
            >
              {conditionalOverride ? 'True' : 'False'}
            </div>,
          )}
        </React.Fragment>,
      )}
      <ComponentPreview
        key={`preview-${props.label}`}
        navigatorEntry={props.navigatorEntry}
        color={props.iconColor}
      />
    </React.Fragment>
  )
})

function asConditional(element: ElementInstanceMetadata | null): JSXConditionalExpression | null {
  if (
    element == null ||
    isLeft(element.element) ||
    !isJSXConditionalExpression(element.element.value)
  ) {
    return null
  }
  return element.element.value
}
