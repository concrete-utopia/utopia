/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme, Button, Icons, SectionActionSheet } from '../../../uuiui'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { when } from '../../../utils/react-conditionals'
import { getMetadata } from '../../editor/store/editor-state'

interface NavigatorHintProps {
  shouldBeShown: boolean
  getMarginForHint: () => number
}

export const NavigatorHintTop: React.FunctionComponent<
  React.PropsWithChildren<NavigatorHintProps>
> = React.memo((props) => {
  const colorTheme = useColorTheme()
  if (props.shouldBeShown) {
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
})

export const NavigatorHintBottom: React.FunctionComponent<
  React.PropsWithChildren<NavigatorHintProps>
> = React.memo((props) => {
  const colorTheme = useColorTheme()
  if (props.shouldBeShown) {
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
})

interface VisiblityIndicatorProps {
  shouldShow: boolean
  visibilityEnabled: boolean
  selected: boolean
  onClick: () => void
}

export const VisibilityIndicator: React.FunctionComponent<
  React.PropsWithChildren<VisiblityIndicatorProps>
> = React.memo((props) => {
  const color = props.selected ? 'on-highlight-main' : 'main'

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
})

type SelectionLocked = 'locked' | 'locked-and-descendants-locked-too' | 'selectable'
interface SelectionLockedIndicatorProps {
  shouldShow: boolean
  value: SelectionLocked
  selected: boolean
  isDescendantOfLocked: boolean
  onClick: (value: SelectionLocked) => void
}

export const SelectionLockedIndicator: React.FunctionComponent<
  React.PropsWithChildren<SelectionLockedIndicatorProps>
> = React.memo((props) => {
  const color = props.selected ? 'on-highlight-main' : 'main'

  const handleClick = React.useCallback(() => {
    switch (props.value) {
      case 'selectable':
        props.onClick('locked-and-descendants-locked-too')
        break
      case 'locked-and-descendants-locked-too':
        props.onClick('locked')
        break
      case 'locked':
      default:
        props.onClick('selectable')
        break
    }
  }, [props])
  return (
    <Button
      onClick={handleClick}
      style={{
        marginRight: 4,
        height: 18,
        width: 18,
        opacity: props.shouldShow ? 1 : 0,
      }}
    >
      {when(props.value === 'locked', <Icons.LockClosed color={color} />)}
      {when(props.value === 'locked-and-descendants-locked-too', <Icons.Gear color={color} />)}
      {when(
        props.value === 'selectable' && !props.isDescendantOfLocked,
        <Icons.LockOpen color={color} style={{ transform: 'scale(.85)' }} />,
      )}
      {when(
        props.value === 'selectable' && props.isDescendantOfLocked,
        <span style={{ fontSize: '20px', fontWeight: 'bold' }}>·</span>,
      )}
    </Button>
  )
})

interface OriginalComponentNameLabelProps {
  selected: boolean
  instanceOriginalComponentName: string | null
}

export const OriginalComponentNameLabel: React.FunctionComponent<
  React.PropsWithChildren<OriginalComponentNameLabelProps>
> = React.memo((props) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        fontStyle: 'normal',
        paddingRight: 4,
        paddingLeft: 4,
        fontSize: 10,
        color: props.selected ? colorTheme.white.value : colorTheme.navigatorComponentName.value,
        display: props.instanceOriginalComponentName == null ? 'none' : undefined,
      }}
    >
      {props.instanceOriginalComponentName}
    </div>
  )
})

interface NavigatorItemActionSheetProps {
  selected: boolean
  highlighted: boolean
  elementPath: ElementPath
  isVisibleOnCanvas: boolean // TODO FIXME bad name, also, use state
  instanceOriginalComponentName: string | null
  dispatch: EditorDispatch
}

export const NavigatorItemActionSheet: React.FunctionComponent<
  React.PropsWithChildren<NavigatorItemActionSheetProps>
> = React.memo((props) => {
  const { elementPath, dispatch } = props

  const toggleHidden = React.useCallback(() => {
    dispatch([EditorActions.toggleHidden([elementPath])], 'everyone')
  }, [dispatch, elementPath])

  const toggleSelectable = React.useCallback(
    (newValue: SelectionLocked) => {
      dispatch([EditorActions.toggleSelectionLock([elementPath], newValue)], 'everyone')
    },
    [dispatch, elementPath],
  )
  const isLockedElement = useEditorState((store) => {
    return store.editor.lockedElements.some((path) => EP.pathsEqual(elementPath, path))
  }, 'NavigatorItemActionSheet locked')
  const isLockedAndDescendants = useEditorState((store) => {
    return store.editor.lockedElementsAndDescendants.some((path) =>
      EP.pathsEqual(elementPath, path),
    )
  }, 'NavigatorItemActionSheet really locked')

  const jsxMetadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const isSceneElement = React.useMemo(
    () => MetadataUtils.isProbablyScene(jsxMetadataRef.current, elementPath),
    [elementPath, jsxMetadataRef],
  )

  const isDescendantOfLocked = useEditorState((store) => {
    return store.editor.lockedElementsAndDescendants.some((path) =>
      EP.isDescendantOf(elementPath, path),
    )
  }, 'NavigatorItemActionSheet descendant of locked ·')

  return (
    <SectionActionSheet>
      <OriginalComponentNameLabel
        selected={props.selected}
        instanceOriginalComponentName={props.instanceOriginalComponentName}
      />
      <SelectionLockedIndicator
        key={`selection-locked-indicator-${EP.toVarSafeComponentId(elementPath)}`}
        shouldShow={
          !isSceneElement &&
          (props.highlighted ||
            props.selected ||
            isLockedElement ||
            isLockedAndDescendants ||
            isDescendantOfLocked)
        }
        value={
          isLockedElement
            ? 'locked'
            : isLockedAndDescendants
            ? 'locked-and-descendants-locked-too'
            : 'selectable'
        }
        isDescendantOfLocked={isDescendantOfLocked}
        selected={props.selected}
        onClick={toggleSelectable}
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
})
