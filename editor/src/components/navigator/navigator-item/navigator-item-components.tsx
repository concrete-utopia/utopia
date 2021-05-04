/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import { DropTargetType } from '../navigator'
import { colorTheme, Button, Icons, SectionActionSheet } from '../../../uuiui'

interface NavigatorHintProps {
  isOver: boolean
  dropTargetType: DropTargetType
  getMarginForHint: () => number
}

export const NavigatorHintTop: React.FunctionComponent<NavigatorHintProps> = (props) => {
  if (props.isOver && props.dropTargetType != null && props.dropTargetType === 'after') {
    return (
      <div
        style={{
          marginLeft: props.getMarginForHint(),
          backgroundColor: colorTheme.navigatorResizeHintBorder.value,
          height: 2,
          position: 'absolute',
          top: 0,
          width: '100%',
          borderRadius: '2px',
          overflow: 'hidden',
        }}
      />
    )
  } else {
    return null
  }
}
NavigatorHintTop.displayName = 'NavigatorHintTop'

export const NavigatorHintBottom: React.FunctionComponent<NavigatorHintProps> = (props) => {
  if (props.isOver && props.dropTargetType != null && props.dropTargetType === 'before') {
    return (
      <div
        style={{
          marginLeft: props.getMarginForHint(),
          backgroundColor: colorTheme.navigatorResizeHintBorder.value,
          height: 2,
          position: 'absolute',
          bottom: 0,
          width: '100%',
          borderRadius: '2px',
          overflow: 'hidden',
        }}
      />
    )
  } else {
    return null
  }
}
NavigatorHintBottom.displayName = 'NavigatorHintBottom'

interface VisiblityIndicatorProps {
  shouldShow: boolean
  visibilityEnabled: boolean
  selected: boolean
  onClick: () => void
}

export const VisibilityIndicator: React.FunctionComponent<VisiblityIndicatorProps> = (props) => {
  const color = props.selected ? 'white' : 'gray'

  return (
    <Button
      onClick={props.onClick}
      style={{
        marginRight: 4,
        height: 18,
        width: 18,
        opacity: props.shouldShow ? 1 : 0,
      }}
    >
      {props.visibilityEnabled ? (
        <Icons.EyeOpen color={color} style={{ transform: 'scale(.85)' }} />
      ) : (
        <Icons.EyeStrikethrough color={color} />
      )}
    </Button>
  )
}
VisibilityIndicator.displayName = 'VisibilityIndicator'

interface OriginalComponentNameLabelProps {
  selected: boolean
  instanceOriginalComponentName: string | null
}

export const OriginalComponentNameLabel: React.FunctionComponent<OriginalComponentNameLabelProps> = (
  props,
) => {
  return (
    <div
      className={'role-componentnanme pr4 pl4 f10 '}
      style={{
        fontStyle: 'normal',
        color: props.selected ? colorTheme.white.value : colorTheme.navigatorComponentName.value,
        display: props.instanceOriginalComponentName == null ? 'none' : undefined,
      }}
    >
      {props.instanceOriginalComponentName}
    </div>
  )
}
OriginalComponentNameLabel.displayName = 'OriginalComponentNameLabel'

interface NavigatorItemActionSheetProps {
  selected: boolean
  highlighted: boolean
  elementPath: ElementPath
  isVisibleOnCanvas: boolean // TODO FIXME bad name, also, use state
  instanceOriginalComponentName: string | null
  dispatch: EditorDispatch
}

export const NavigatorItemActionSheet: React.FunctionComponent<NavigatorItemActionSheetProps> = (
  props,
) => {
  const { elementPath, dispatch } = props

  const toggleHidden = React.useCallback(() => {
    dispatch([EditorActions.toggleHidden([elementPath])], 'everyone')
  }, [dispatch, elementPath])
  return (
    <SectionActionSheet>
      <OriginalComponentNameLabel
        selected={props.selected}
        instanceOriginalComponentName={props.instanceOriginalComponentName}
      />
      <VisibilityIndicator
        key={`visibility-indicator-${EP.toVarSafeComponentId(elementPath)}`}
        shouldShow={props.highlighted || props.selected || !props.isVisibleOnCanvas}
        visibilityEnabled={props.isVisibleOnCanvas}
        selected={props.selected}
        onClick={toggleHidden}
      />
    </SectionActionSheet>
  )
}
NavigatorItemActionSheet.displayName = 'NavigatorItemActionSheet'
