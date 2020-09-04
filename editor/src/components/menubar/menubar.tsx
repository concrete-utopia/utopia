/** @jsx jsx */
import { jsx } from '@emotion/core'
import styled from '@emotion/styled'
import { UtopiaStyles } from 'uuiui'
import * as React from 'react'
import { Avatar } from 'uuiui'
import { IcnProps } from 'uuiui'
import { LeftMenuTab } from '../navigator/left-pane'
import { EditorState } from '../editor/store/editor-state'
import { LoginState } from '../../common/user'
import { EditorDispatch, EditorAction } from '../editor/action-types'
import { FlexColumn } from 'uuiui'
import { MenuIcons } from 'uuiui'
import {
  setLeftMenuTab,
  setLeftMenuExpanded,
  togglePanel,
  setPanelVisibility,
} from '../editor/actions/actions'

import { useEditorState } from '../editor/store/store-hook'
import { SquareButton } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { shareURLForProject } from '../../core/shared/utils'

interface MenuBarProps {
  editorState: EditorState
  loginState: LoginState
  editorDispatch: EditorDispatch
  expandedLeftPanel: boolean
}

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

  const handleOnMouseOver = () => setHovered(true)
  const handleOnMouseOut = () => setHovered(false)

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
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      selectedTab: store.editor.leftMenu.selectedTab,
      userState: store.userState,
      leftMenuExpanded: store.editor.leftMenu.expanded,
      projectId: store.editor.id,
      projectName: store.editor.projectName,
    }
  })

  const onShow = React.useCallback(
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

  const onShowNavigateTab = React.useCallback(() => {
    onShow(LeftMenuTab.UINavigate)
  }, [onShow])

  const onShowStructureTab = React.useCallback(() => {
    onShow(LeftMenuTab.ProjectStructure)
  }, [onShow])

  const onShowProjectTab = React.useCallback(() => {
    onShow(LeftMenuTab.ProjectSettings)
  }, [onShow])

  const onToggleMenu = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

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
        <MenuTile
          selected={selectedTab === LeftMenuTab.ProjectStructure}
          menuExpanded={leftMenuExpanded}
          icon={<MenuIcons.Menu />}
          onClick={onShowStructureTab}
        />

        <MenuTile
          selected={selectedTab === LeftMenuTab.UINavigate}
          menuExpanded={leftMenuExpanded}
          icon={<MenuIcons.Project />}
          onClick={onShowNavigateTab}
        />
        <a target='_blank' rel='noopener noreferrer' href={previewURL}>
          <MenuTile selected={false} menuExpanded={false} icon={<MenuIcons.ExternalLink />} />
        </a>
      </FlexColumn>
      <FlexColumn>
        <Tile>
          <a href='/projects'>
            <Avatar loginState={userState.loginState} size={28} />
          </a>
        </Tile>
      </FlexColumn>
    </FlexColumn>
  )
})
Menubar.displayName = 'Menubar'
