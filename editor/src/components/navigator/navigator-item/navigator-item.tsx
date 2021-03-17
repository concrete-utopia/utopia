/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { JSXElementName } from '../../../core/shared/element-template'
import { ElementOriginType, TemplatePath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as TP from '../../../core/shared/template-path'
import { ExpandableIndicator } from './expandable-indicator'
import { ItemLabel } from './item-label'
import { ComponentPreview } from './component-preview'
import { NavigatorItemActionSheet } from './navigator-item-components'
import { useScrollToThisIfSelected } from './scroll-to-element-if-selected-hook'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import { WarningIcon } from '../../../uuiui/warning-icon'
import { ElementWarnings } from '../../editor/store/editor-state'
import { ChildWithPercentageSize } from '../../common/size-warnings'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../utils/react-performance'
import { IcnProps, colorTheme, UtopiaStyles, UtopiaTheme, FlexRow } from '../../../uuiui'
import { LayoutIcon } from './layout-icon'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

interface ComputedLook {
  style: React.CSSProperties
  iconColor: IcnProps['color']
}

export const BasePaddingUnit = 20

export function getElementPadding(templatePath: TemplatePath): number {
  // if an element is the child of the Storyboard component, let's show it at the root level
  const extraDepthRemoval = TP.pathsEqual(
    TP.scenePathPartOfTemplatePath(templatePath),
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
  label: string
  staticElementName: JSXElementName | null
  dispatch: EditorDispatch
  isHighlighted: boolean
  collapsed: boolean
  isElementVisible: boolean
  renamingTarget: TemplatePath | null
  selected: boolean
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

function useStyleFullyVisible(path: TemplatePath): boolean {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadataKILLME
    const selectedViews = store.editor.selectedViews
    const isSelected = selectedViews.some((selected) => TP.pathsEqual(path, selected))
    const isParentOfSelected = selectedViews.some((selected) =>
      TP.pathsEqual(path, TP.parentPath(selected)),
    )
    const isScenePath = TP.isScenePath(path)

    const isContainingBlockAncestor = selectedViews.some((selected) => {
      return TP.pathsEqual(MetadataUtils.findContainingBlock(metadata.elements, selected), path)
    })

    const isFlexAncestorDirectionChange = selectedViews.some((selected) => {
      const selectedSizeMeasurements = TP.isInstancePath(selected)
        ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, selected)
            ?.specialSizeMeasurements
        : null
      const parentPath = TP.parentPath(selected)
      if (
        selectedSizeMeasurements?.parentLayoutSystem === 'flex' &&
        !isParentOfSelected &&
        TP.isAncestorOf(selected, path) &&
        parentPath != null
      ) {
        const flexDirectionChange = MetadataUtils.findNearestAncestorFlexDirectionChange(
          metadata.elements,
          parentPath,
        )
        return TP.pathsEqual(flexDirectionChange, path)
      } else {
        return false
      }
    })

    return (
      isScenePath ||
      isSelected ||
      isParentOfSelected ||
      isContainingBlockAncestor ||
      isFlexAncestorDirectionChange
    )
  }, 'NavigatorItem useStyleFullyVisible')
}

export const NavigatorItem: React.FunctionComponent<NavigatorItemInnerProps> = betterReactMemo(
  'NavigatorItem',
  (props) => {
    const {
      label,
      dispatch,
      isHighlighted,
      isElementVisible,
      renamingTarget,
      selected,
      collapsed,
      elementOriginType,
      templatePath,
      getSelectedViewsInRange,
      index,
      elementWarnings,
    } = props

    const domElementRef = useScrollToThisIfSelected(selected)

    const childComponentCount = props.noOfChildren

    const isDynamic =
      elementOriginType === 'unknown-element' ||
      elementOriginType === 'generated-static-definition-present'

    const isScene = TP.isScenePath(templatePath)
    const fullyVisible = useStyleFullyVisible(templatePath)

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

      // additional style
      result.style = {
        ...result.style,
        fontWeight: isScene ? 500 : 'inherit',
        opacity: fullyVisible ? 1 : 0.5,
      }

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

    const layoutIcon = (
      <LayoutIcon
        key={`layout-type-${label}`}
        path={templatePath}
        color={resultingStyle.iconColor}
        warningText={warningText}
      />
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
          {layoutIcon}
          <ItemLabel
            key={`label-${label}`}
            testId={`navigator-item-label-${label}`}
            name={label}
            isDynamic={isDynamic}
            target={templatePath}
            canRename={selected}
            dispatch={dispatch}
            inputVisible={TP.pathsEqual(renamingTarget, templatePath)}
            elementOriginType={elementOriginType}
          />
          <ComponentPreview
            key={`preview-${label}`}
            path={templatePath}
            color={resultingStyle.iconColor}
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
