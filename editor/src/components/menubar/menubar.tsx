/** @jsx jsx */
import { jsx } from '@emotion/core'
import styled from '@emotion/styled'
import * as React from 'react'
import {
  Avatar,
  FlexColumn,
  IcnProps,
  LargerIcons,
  MenuIcons,
  SquareButton,
  Tooltip,
  UtopiaStyles,
} from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { LoginState } from '../../common/user'
import { shareURLForProject } from '../../core/shared/utils'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import { setLeftMenuTab, setPanelVisibility, togglePanel } from '../editor/actions/actions'
import { EditorState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { LeftMenuTab } from '../navigator/left-pane'

const Tile = styled.div({
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
})

export interface MenuTileProps extends React.HTMLAttributes<HTMLDivElement> {
  selected: boolean
  menuExpanded: boolean
  icon: React.ReactElement<IcnProps>
}

// export const Button = styled.div<ButtonProps>((props: ButtonProps) => ({

export const MenuTile: React.FunctionComponent<MenuTileProps> = (props) => {
  const [hovered, setHovered] = React.useState(false)

  const handleOnMouseOver = React.useCallback(() => setHovered(true), [])
  const handleOnMouseOut = React.useCallback(() => setHovered(false), [])

  var foregroundColor: IcnProps['color'] = 'black'
  if (props.menuExpanded && props.selected) {
    foregroundColor = 'white'
  } else if (props.selected || hovered) {
    foregroundColor = 'blue'
  }

  return (
    <Tile
      style={{ width: 44, height: 44 }}
      onMouseOver={handleOnMouseOver}
      onMouseOut={handleOnMouseOut}
      onClick={props.onClick}
    >
      <SquareButton
        highlight
        style={{
          ...props.style,
          borderRadius: 1,
          width: 28,
          height: 28,
          background:
            props.menuExpanded && props.selected ? UtopiaStyles.backgrounds.blue : 'transparent',
        }}
      >
        {React.cloneElement(props.icon, {
          color: foregroundColor,
        })}
      </SquareButton>
    </Tile>
  )
}

export const Menubar = betterReactMemo('Menubar', () => {
  const {
    dispatch,
    selectedTab,
    userState,
    leftMenuExpanded,
    projectId,
    projectName,
    isPreviewPaneVisible,
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      selectedTab: store.editor.leftMenu.selectedTab,
      userState: store.userState,
      leftMenuExpanded: store.editor.leftMenu.expanded,
      projectId: store.editor.id,
      projectName: store.editor.projectName,
      isPreviewPaneVisible: store.editor.preview.visible,
    }
  }, 'Menubar')

  const onClickTab = React.useCallback(
    (menuTab: LeftMenuTab) => {
      let actions: Array<EditorAction> = []
      if (selectedTab === menuTab) {
        actions.push(togglePanel('leftmenu'))
      } else {
        actions.push(setPanelVisibility('leftmenu', true))
      }
      actions.push(setLeftMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch, selectedTab],
  )

  const onDoubleClick = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

  const onClickNavigateTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.UINavigate)
  }, [onClickTab])

  const onClickStructureTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.ProjectStructure)
  }, [onClickTab])

  const togglePreviewPaneVisible = React.useCallback(
    () => dispatch([setPanelVisibility('preview', !isPreviewPaneVisible)]),
    [dispatch, isPreviewPaneVisible],
  )

  const previewURL =
    projectId == null ? '' : shareURLForProject(FLOATING_PREVIEW_BASE_URL, projectId, projectName)

  return (
    <FlexColumn
      id='leftMenuBar'
      style={{
        cursor: 'pointer',
        flexGrow: 1,
      }}
      onDoubleClick={onDoubleClick}
    >
      <FlexColumn style={{ flexGrow: 1 }}>
        <Tooltip title={'Project Structure'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.ProjectStructure}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Menu />}
              onClick={onClickStructureTab}
            />
          </span>
        </Tooltip>

        <Tooltip title={'Navigator'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.UINavigate}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Project />}
              onClick={onClickNavigateTab}
            />
          </span>
        </Tooltip>
        <a target='_blank' rel='noopener noreferrer' href={previewURL}>
          <Tooltip title={'Launch External Preview'} placement={'right'}>
            <span>
              <MenuTile selected={false} menuExpanded={false} icon={<MenuIcons.ExternalLink />} />
            </span>
          </Tooltip>
        </a>
        <Tooltip title={'Embedded Preview'} placement={'right'}>
          <span>
            <MenuTile
              selected={isPreviewPaneVisible}
              menuExpanded={false}
              icon={<LargerIcons.PreviewPane />}
              onClick={togglePreviewPaneVisible}
            />
          </span>
        </Tooltip>
      </FlexColumn>
      <Tile style={{ marginTop: 12, marginBottom: 12 }}>
        <a href='/projects'>
          <Avatar loginState={userState.loginState} size={28} />
        </a>
      </Tile>
    </FlexColumn>
  )
})
Menubar.displayName = 'Menubar'
