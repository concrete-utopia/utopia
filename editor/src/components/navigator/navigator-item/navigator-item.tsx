/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
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
import { ElementWarnings } from '../../editor/store/editor-state'
import { ChildWithPercentageSize } from '../../common/size-warnings'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { IcnProps, useColorTheme, UtopiaStyles, UtopiaTheme, FlexRow } from '../../../uuiui'
import { LayoutIcon } from './layout-icon'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

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

const componentSelected = (colorTheme: any): ComputedLook => ({
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
  colorTheme: any,
) => {
  let result = defaultUnselected(colorTheme)
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
  } else {
    // unselected
    if (isInsideComponent) {
      result = componentUnselected(colorTheme)
    } else if (isDynamic) {
      result = dynamicUnselected(colorTheme)
    } else {
      result = defaultUnselected(colorTheme)
    }
  }

  let boxShadow: string | undefined = undefined
  if (isProbablyScene) {
    boxShadow = `inset 0 -1px ${colorTheme.subduedBorder.value}`
  } else if (isFocusedComponent) {
    boxShadow = `inset 0 1px ${colorTheme.subduedBorder.value}`
  }

  // additional style
  result.style = {
    ...result.style,
    fontWeight: isProbablyScene || fullyVisible ? 500 : 'inherit',
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

function useIsProbablyScene(path: ElementPath): boolean {
  return useEditorState(
    (store) => MetadataUtils.isProbablySceneFromMetadata(store.editor.jsxMetadata, path),
    'NavigatorItem useIsProbablyScene',
  )
}

export const NavigatorItem: React.FunctionComponent<NavigatorItemInnerProps> = React.memo(
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

    const colorTheme = useColorTheme()
    const isFocusedComponent = useEditorState(
      (store) => EP.isFocused(store.editor.focusedElementPath, elementPath),
      'NavigatorItem isFocusedComponent',
    )

    const isFocusableComponent = useEditorState((store) => {
      return MetadataUtils.isFocusableComponent(elementPath, store.editor.jsxMetadata)
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
      colorTheme,
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
    const focusComponent = React.useCallback(() => {
      if (isFocusableComponent) {
        dispatch([EditorActions.setFocusedElement(elementPath)])
      }
    }, [dispatch, elementPath, isFocusableComponent])

    const containerStyle: React.CSSProperties = React.useMemo(() => {
      return {
        opacity: isElementVisible ? undefined : 0.5,
        overflow: 'hidden',
        flexGrow: 1,
        flexShrink: 0,
      }
    }, [isElementVisible])

    const rowStyle = useKeepReferenceEqualityIfPossible({
      paddingLeft: getElementPadding(elementPath),
      height: UtopiaTheme.layout.rowHeight.smaller,
      ...resultingStyle.style,
    })

    return (
      <FlexRow
        style={rowStyle}
        onMouseDown={select}
        onMouseMove={highlight}
        onDoubleClick={focusComponent}
      >
        <FlexRow style={containerStyle}>
          <ExpandableIndicator
            key='expandable-indicator'
            visible={childComponentCount > 0 || isFocusedComponent}
            collapsed={collapsed}
            selected={selected && !isInsideComponent}
            onMouseDown={collapse}
            style={{ transform: 'scale(0.8)', opacity: 0.5 }}
          />
          <NavigatorRowLabel
            elementPath={elementPath}
            label={props.label}
            renamingTarget={props.renamingTarget}
            selected={props.selected}
            elementOriginType={props.elementOriginType}
            dispatch={props.dispatch}
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

interface NavigatorRowLabelProps {
  elementPath: ElementPath
  iconColor: IcnProps['color']
  warningText: string | null
  label: string
  isDynamic: boolean
  renamingTarget: ElementPath | null
  selected: boolean
  elementOriginType: ElementOriginType
  dispatch: EditorDispatch
}

const NavigatorRowLabel = React.memo((props: NavigatorRowLabelProps) => {
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
})
