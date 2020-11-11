/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { colorTheme, FlexRow, IcnProps, Icons, Tooltip, UtopiaStyles, UtopiaTheme } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { ElementInstanceMetadata, JSXElementName } from '../../../core/shared/element-template'
import { ElementOriginType, Imports, TemplatePath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/actions'
import { useKeepReferenceEqualityIfPossible } from '../../inspector/common/property-path-hooks'
import * as TP from '../../../core/shared/template-path'
import { ExpandableIndicator } from './expandable-indicator'
import { ItemLabel } from './item-label'
import { ItemPreview } from './item-preview'
import { NavigatorItemActionSheet } from './navigator-item-components'
import { useScrollToThisIfSelected } from './scroll-to-element-if-selected-hook'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import { WarningIcon } from '../../../uuiui/warning-icon'
import { ElementWarnings } from '../../editor/store/editor-state'
import { ChildWithPercentageSize } from '../../common/size-warnings'

interface ComputedLook {
  style: React.CSSProperties
  iconColor: IcnProps['color']
}

export const BasePaddingUnit = 20

export function getElementPadding(templatePath: TemplatePath): number {
  // if an element is the child of the Storyboard component, let's show it at the root level
  const extraDepthRemoval = TP.pathsEqual(
    TP.scenePathForPath(templatePath),
    EmptyScenePathForStoryboard,
  )
  const depthOffset = extraDepthRemoval ? 2 : 0
  return (TP.depth(templatePath) - 1 - depthOffset) * BasePaddingUnit
}

export interface NavigatorItemInnerProps {
  templatePath: TemplatePath
  index: number
  getSelectedViewsInRange: (i: number) => Array<TemplatePath> // TODO KILLME
  noOfChildren: number
  isAutosizingView: boolean
  label: string
  isFlexLayoutedContainer: boolean
  yogaDirection: 'row' | 'row-reverse' | 'column' | 'column-reverse'
  yogaWrap: 'wrap' | 'wrap-reverse' | 'nowrap'
  staticElementName: JSXElementName | null
  componentInstance: boolean
  dispatch: EditorDispatch
  isHighlighted: boolean
  collapsed: boolean
  isElementVisible: boolean
  renamingTarget: TemplatePath | null
  selected: boolean
  imports: Imports
  elementOriginType: ElementOriginType
  elementWarnings: ElementWarnings
}

function selectItem(
  dispatch: EditorDispatch,
  getSelectedViewsInRange: (i: number) => Array<TemplatePath>,
  templatePath: TemplatePath,
  index: number,
  selected: boolean,
  event: React.MouseEvent<HTMLDivElement>,
) {
  if (!selected) {
    if (event.metaKey && !event.shiftKey) {
      // adds to selection
      dispatch([EditorActions.selectComponents([templatePath], true)], 'leftpane')
    } else if (event.shiftKey) {
      // selects range of items
      const targets = getSelectedViewsInRange(index)
      dispatch([EditorActions.selectComponents(targets, false)], 'leftpane')
    } else {
      dispatch([EditorActions.selectComponents([templatePath], false)], 'leftpane')
    }
  }
}

const highlightItem = (
  dispatch: EditorDispatch,
  templatePath: TemplatePath,
  selected: boolean,
  highlighted: boolean,
) => {
  if (!highlighted) {
    if (selected) {
      dispatch([EditorActions.clearHighlightedViews()], 'leftpane')
    } else {
      dispatch([EditorActions.setHighlightedView(templatePath)], 'leftpane')
    }
  }
}

const collapseItem = (
  dispatch: EditorDispatch,
  templatePath: TemplatePath,
  e: React.MouseEvent<HTMLDivElement>,
) => {
  dispatch([EditorActions.toggleCollapse(templatePath)], 'leftpane')
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

const instanceDefaultUnselected: ComputedLook = {
  style: { background: 'transparent', color: colorTheme.brandPurple.value },
  iconColor: 'purple',
}
const instanceDefaultSelectedStyle: ComputedLook = {
  style: { background: UtopiaStyles.backgrounds.lightblue, color: colorTheme.white.value },
  iconColor: 'white',
}

const instanceDynamicUnselected: ComputedLook = {
  style: { background: 'transparent', color: colorTheme.brandPurple.value },
  iconColor: 'purple',
}
const instanceDynamicSelected: ComputedLook = {
  style: { background: UtopiaStyles.backgrounds.lightblue, color: colorTheme.white.value },
  iconColor: 'purple',
}

export const NavigatorItem: React.FunctionComponent<NavigatorItemInnerProps> = betterReactMemo(
  'NavigatorItem',
  (props) => {
    const {
      staticElementName,
      label,
      isAutosizingView,
      dispatch,
      isHighlighted,
      isElementVisible,
      renamingTarget,
      selected,
      collapsed,
      imports,
      elementOriginType,
      templatePath,
      getSelectedViewsInRange,
      index,
      elementWarnings,
      isFlexLayoutedContainer,
      yogaDirection,
      yogaWrap,
    } = props

    const domElementRef = useScrollToThisIfSelected(selected)

    const childComponentCount = props.noOfChildren

    const isDynamic =
      elementOriginType === 'unknown-element' ||
      elementOriginType === 'generated-static-definition-present'

    const isScene = TP.isScenePath(templatePath)

    const computeResultingStyle = () => {
      let result = defaultUnselected
      if (selected) {
        if (isDynamic) {
          result = dynamicSelected
        } else {
          result = defaultSelected
        }
      } else {
        // unselected
        if (isDynamic) {
          result = dynamicUnselected
        } else {
          result = defaultUnselected
        }
      }

      // additional style for scenes
      result.style = { ...result.style, fontWeight: isScene ? 500 : 'inherit' }

      return result
    }

    const resultingStyle = computeResultingStyle()

    let warningText: string | null = null
    if (elementWarnings.dynamicSceneChildWidthHeightPercentage) {
      warningText = ChildWithPercentageSize
    } else if (elementWarnings.widthOrHeightZero) {
      warningText = 'Missing width or height'
    } else if (elementWarnings.absoluteWithUnpositionedParent) {
      warningText = 'Element is trying to be position absolutely with an unconfigured parent'
    }

    const preview =
      warningText == null ? (
        <ItemPreview
          key={`preview-${label}`}
          {...props}
          isAutosizingView={isAutosizingView}
          collapsed={collapsed}
          isFlexLayoutedContainer={isFlexLayoutedContainer}
          yogaDirection={yogaDirection}
          yogaWrap={yogaWrap}
          color={resultingStyle.iconColor}
          imports={imports}
        />
      ) : (
        <WarningIcon tooltipText={warningText} />
      )

    const collapse = React.useCallback(
      (event: any) => collapseItem(dispatch, templatePath, event),
      [dispatch, templatePath],
    )
    const select = React.useCallback(
      (event: any) =>
        selectItem(dispatch, getSelectedViewsInRange, templatePath, index, selected, event),
      [dispatch, getSelectedViewsInRange, templatePath, index, selected],
    )
    const highlight = React.useCallback(
      () => highlightItem(dispatch, templatePath, selected, isHighlighted),
      [dispatch, templatePath, selected, isHighlighted],
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
      paddingLeft: getElementPadding(templatePath),
      height: UtopiaTheme.layout.rowHeight.smaller,
      ...resultingStyle.style,
    })

    return (
      <FlexRow ref={domElementRef} style={rowStyle} onMouseDown={select} onMouseMove={highlight}>
        <FlexRow style={containerStyle}>
          <ExpandableIndicator
            key='expandable-indicator'
            visible={childComponentCount > 0}
            collapsed={collapsed}
            selected={selected}
            onMouseDown={collapse}
          />
          {preview}
          <ItemLabel
            key={`label-${label}`}
            name={label}
            isDynamic={isDynamic}
            target={templatePath}
            canRename={selected}
            dispatch={dispatch}
            inputVisible={TP.pathsEqual(renamingTarget, templatePath)}
            elementOriginType={elementOriginType}
          />
        </FlexRow>
        <NavigatorItemActionSheet
          templatePath={templatePath}
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
