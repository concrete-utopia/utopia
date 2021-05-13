/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import {
  ElementInstanceMetadataMap,
  isJSXElement,
  JSXElementName,
} from '../../../core/shared/element-template'
import { ElementOriginType, ElementPath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import { ExpandableIndicator } from './expandable-indicator'
import { ItemLabel } from './item-label'
import { ComponentPreview } from './component-preview'
import { NavigatorItemActionSheet } from './navigator-item-components'
import { useScrollToThisIfSelected } from './scroll-to-element-if-selected-hook'
import {
  ElementWarnings,
  getJSXComponentsAndImportsForPathInnerComponentFromState,
} from '../../editor/store/editor-state'
import { ChildWithPercentageSize } from '../../common/size-warnings'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../utils/react-performance'
import { IcnProps, colorTheme, UtopiaStyles, UtopiaTheme, FlexRow } from '../../../uuiui'
import { LayoutIcon } from './layout-icon'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isSceneElementIgnoringImports } from '../../../core/model/scene-utils'
import { isRight } from '../../../core/shared/either'

interface ComputedLook {
  style: React.CSSProperties
  iconColor: IcnProps['color']
}

export const BasePaddingUnit = 20

export function getElementPadding(elementPath: ElementPath): number {
  return EP.navigatorDepth(elementPath) * BasePaddingUnit
}

export interface NavigatorItemInnerProps {
  elementPath: ElementPath
  index: number
  getSelectedViewsInRange: (i: number) => Array<ElementPath> // TODO KILLME
  noOfChildren: number
  label: string
  staticElementName: JSXElementName | null
  dispatch: EditorDispatch
  isHighlighted: boolean
  collapsed: boolean
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  selected: boolean
  elementOriginType: ElementOriginType
  elementWarnings: ElementWarnings
}

function selectItem(
  dispatch: EditorDispatch,
  getSelectedViewsInRange: (i: number) => Array<ElementPath>,
  elementPath: ElementPath,
  index: number,
  selected: boolean,
  event: React.MouseEvent<HTMLDivElement>,
) {
  if (!selected) {
    if (event.metaKey && !event.shiftKey) {
      // adds to selection
      dispatch([EditorActions.selectComponents([elementPath], true)], 'leftpane')
    } else if (event.shiftKey) {
      // selects range of items
      const targets = getSelectedViewsInRange(index)
      dispatch([EditorActions.selectComponents(targets, false)], 'leftpane')
    } else {
      dispatch([EditorActions.selectComponents([elementPath], false)], 'leftpane')
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

const defaultUnselected: ComputedLook = {
  style: { background: 'transparent', color: colorTheme.neutralForeground.value },
  iconColor: 'black',
}
const defaultSelected: ComputedLook = {
  style: { background: UtopiaStyles.backgrounds.blue, color: colorTheme.white.value },
  iconColor: 'white',
}

const dynamicUnselected: ComputedLook = {
  style: { background: 'transparent', color: colorTheme.primary.value },
  iconColor: 'blue',
}
const dynamicSelected: ComputedLook = {
  style: { background: UtopiaStyles.backgrounds.lightblue, color: colorTheme.white.value },
  iconColor: 'white',
}

const componentUnselected: ComputedLook = {
  style: {
    background: colorTheme.emphasizedBackground.value,
    color: colorTheme.neutralForeground.value,
  },
  iconColor: 'orange',
}
const componentSelected: ComputedLook = {
  style: {
    background: colorTheme.navigatorComponentSelected.value,
    color: colorTheme.neutralForeground.value,
  },
  iconColor: 'orange',
}

const computeResultingStyle = (
  selected: boolean,
  isInsideComponent: boolean,
  isDynamic: boolean,
  isProbablyScene: boolean,
  fullyVisible: boolean,
  isFocusedComponent: boolean,
  isFocusableComponent: boolean,
) => {
  let result = defaultUnselected
  if (selected) {
    if (isFocusableComponent && !isFocusedComponent) {
      result = {
        style: { backgroundColor: colorTheme.brandPurple.value, color: colorTheme.white.value },
        iconColor: 'white',
      }
    } else if (isInsideComponent) {
      result = componentSelected
    } else if (isDynamic) {
      result = dynamicSelected
    } else {
      result = defaultSelected
    }
  } else {
    // unselected
    if (isInsideComponent) {
      result = componentUnselected
    } else if (isDynamic) {
      result = dynamicUnselected
    } else {
      result = defaultUnselected
    }
  }

  let boxShadow: string | undefined = undefined
  if (isProbablyScene) {
    boxShadow = `inset 0 -1px ${colorTheme.inputBorder.value}`
  } else if (isFocusedComponent) {
    boxShadow = `inset 0 1px ${colorTheme.inputBorder.value}`
  }

  // additional style
  result.style = {
    ...result.style,
    fontWeight: isProbablyScene ? 500 : 'inherit',
    opacity: fullyVisible ? 1 : 0.5,
    boxShadow: boxShadow,
  }

  return result
}

function useStyleFullyVisible(path: ElementPath): boolean {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadata
    const selectedViews = store.editor.selectedViews
    const isSelected = selectedViews.some((selected) => EP.pathsEqual(path, selected))
    const isParentOfSelected = selectedViews.some((selected) => EP.isParentOf(path, selected))

    const isStoryboardChild = EP.isStoryboardChild(path)

    const isContainingBlockAncestor = selectedViews.some((selected) => {
      return EP.pathsEqual(MetadataUtils.findContainingBlock(metadata, selected), path)
    })

    const isFlexAncestorDirectionChange = selectedViews.some((selected) => {
      const selectedSizeMeasurements = MetadataUtils.findElementByElementPath(metadata, selected)
        ?.specialSizeMeasurements
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
  }, 'NavigatorItem useStyleFullyVisible')
}

export function isProbablySceneFromMetadata(
  jsxMetadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
  return (
    elementMetadata != null &&
    isRight(elementMetadata.element) &&
    isJSXElement(elementMetadata.element.value) &&
    isSceneElementIgnoringImports(elementMetadata.element.value)
  )
}

function useIsProbablyScene(path: ElementPath): boolean {
  return useEditorState(
    (store) => isProbablySceneFromMetadata(store.editor.jsxMetadata, path),
    'NavigatorItem useIsProbablyScene',
  )
}

export const NavigatorItem: React.FunctionComponent<NavigatorItemInnerProps> = betterReactMemo(
  'NavigatorItem',
  (props) => {
    const {
      dispatch,
      isHighlighted,
      isElementVisible,
      selected,
      collapsed,
      elementOriginType,
      elementPath,
      getSelectedViewsInRange,
      index,
      elementWarnings,
    } = props

    const domElementRef = useScrollToThisIfSelected(selected)
    const isFocusedComponent = useEditorState(
      (store) => EP.isFocused(store.editor.focusedElementPath, elementPath),
      'NavigatorItem isFocusedComponent',
    )

    const isFocusableComponent = useEditorState((store) => {
      const { components } = getJSXComponentsAndImportsForPathInnerComponentFromState(
        elementPath,
        store.editor,
        store.derived,
      )
      return MetadataUtils.isFocusableComponent(elementPath, components, store.editor.jsxMetadata)
    }, 'NavigatorItem isFocusable')

    const childComponentCount = props.noOfChildren

    const isDynamic =
      elementOriginType === 'unknown-element' ||
      elementOriginType === 'generated-static-definition-present'

    const isInsideComponent = EP.isInsideFocusedComponent(elementPath) || isFocusedComponent
    const fullyVisible = useStyleFullyVisible(elementPath)
    const isProbablyScene = useIsProbablyScene(elementPath)

    const resultingStyle = computeResultingStyle(
      selected,
      isInsideComponent,
      isDynamic,
      isProbablyScene,
      fullyVisible,
      isFocusedComponent,
      isFocusableComponent,
    )

    let warningText: string | null = null
    if (elementWarnings.dynamicSceneChildWidthHeightPercentage) {
      warningText = ChildWithPercentageSize
    } else if (elementWarnings.widthOrHeightZero) {
      warningText = 'Missing width or height'
    } else if (elementWarnings.absoluteWithUnpositionedParent) {
      warningText = 'Element is trying to be position absolutely with an unconfigured parent'
    }

    const collapse = React.useCallback((event: any) => collapseItem(dispatch, elementPath, event), [
      dispatch,
      elementPath,
    ])
    const select = React.useCallback(
      (event: any) =>
        selectItem(dispatch, getSelectedViewsInRange, elementPath, index, selected, event),
      [dispatch, getSelectedViewsInRange, elementPath, index, selected],
    )
    const highlight = React.useCallback(
      () => highlightItem(dispatch, elementPath, selected, isHighlighted),
      [dispatch, elementPath, selected, isHighlighted],
    )
    const containerStyle: React.CSSProperties = React.useMemo(() => {
      return {
        opacity: isElementVisible ? undefined : 0.5,
        overflowY: 'hidden',
        overflowX: 'scroll',
        flexGrow: 1,
      }
    }, [isElementVisible])

    const rowStyle = useKeepReferenceEqualityIfPossible({
      paddingLeft: getElementPadding(elementPath),
      height: UtopiaTheme.layout.rowHeight.smaller,
      ...resultingStyle.style,
    })

    return (
      <FlexRow ref={domElementRef} style={rowStyle} onMouseDown={select} onMouseMove={highlight}>
        <FlexRow style={containerStyle}>
          <ExpandableIndicator
            key='expandable-indicator'
            visible={childComponentCount > 0 || isFocusedComponent}
            collapsed={collapsed}
            selected={selected && !isInsideComponent}
            onMouseDown={collapse}
          />
          <NavigatorRowLabel
            {...props}
            collapse={collapse}
            isDynamic={isDynamic}
            iconColor={resultingStyle.iconColor}
            warningText={warningText}
          />
        </FlexRow>
        <NavigatorItemActionSheet
          elementPath={elementPath}
          selected={selected}
          highlighted={isHighlighted}
          isVisibleOnCanvas={isElementVisible}
          instanceOriginalComponentName={null}
          dispatch={dispatch}
        />
      </FlexRow>
    )
  },
)
NavigatorItem.displayName = 'NavigatorItem'

interface NavigatorRowProps extends NavigatorItemInnerProps {
  collapse: (event: any) => void
  isDynamic: boolean
  iconColor: IcnProps['color']
  warningText: string | null
}

const NavigatorRowLabel = (props: NavigatorRowProps) => {
  return (
    <React.Fragment>
      <LayoutIcon
        key={`layout-type-${props.label}`}
        path={props.elementPath}
        color={props.iconColor}
        warningText={props.warningText}
      />
      <ItemLabel
        key={`label-${props.label}`}
        testId={`navigator-item-label-${props.label}`}
        name={props.label}
        isDynamic={props.isDynamic}
        target={props.elementPath}
        canRename={props.selected}
        dispatch={props.dispatch}
        inputVisible={EP.pathsEqual(props.renamingTarget, props.elementPath)}
        elementOriginType={props.elementOriginType}
      />
      <ComponentPreview
        key={`preview-${props.label}`}
        path={props.elementPath}
        color={props.iconColor}
      />
    </React.Fragment>
  )
}
