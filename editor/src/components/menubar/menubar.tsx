/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import { IconMap } from 'antd/lib/result'
import * as React from 'react'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { LoginState } from '../../common/user'
import {
  useTriggerScrollPerformanceTest,
  useTriggerResizePerformanceTest,
  useTriggerSelectionPerformanceTest,
} from '../../core/model/performance-scripts'
import { useReParseOpenProjectFile } from '../../core/model/project-file-helper-hooks'
import { shareURLForProject } from '../../core/shared/utils'
import { isFeatureEnabled } from '../../utils/feature-switches'
import {
  IcnProps,
  SquareButton,
  UtopiaStyles,
  FlexColumn,
  Tooltip,
  MenuIcons,
  LargerIcons,
  Avatar,
  Icn,
  Icons,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import { setLeftMenuTab, setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
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

  const onClickProjectTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Project)
  }, [onClickTab])

  const onClickStoryboardsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Storyboards)
  }, [onClickTab])

  const onClickContentsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Contents)
  }, [onClickTab])

  const onClickSettingsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Settings)
  }, [onClickTab])

  const onClickSharingTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Sharing)
  }, [onClickTab])

  const onClickGithubTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Github)
  }, [onClickTab])

  const togglePreviewPaneVisible = React.useCallback(
    () => dispatch([setPanelVisibility('preview', !isPreviewPaneVisible)]),
    [dispatch, isPreviewPaneVisible],
  )

  const onReparseClick = useReParseOpenProjectFile()

  const onTriggerScrollTest = useTriggerScrollPerformanceTest()
  const onTriggerResizeTest = useTriggerResizePerformanceTest()
  const onTriggerSelectionTest = useTriggerSelectionPerformanceTest()

  const previewURL =
    projectId == null ? '' : shareURLForProject(FLOATING_PREVIEW_BASE_URL, projectId, projectName)

  return (
    <FlexColumn
      id='leftMenuBar'
      style={{
        flexGrow: 1,
      }}
    >
      <FlexColumn style={{ flexGrow: 1 }}>
        <Tooltip title={'Project Structure'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.ProjectStructure}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Menu />}
              onClick={onClickStructureTab}
              style={{ cursor: 'pointer' }}
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
              selected={selectedTab === LeftMenuTab.Storyboards}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Pyramid />}
              onClick={onClickStoryboardsTab}
            />
          </span>
        </Tooltip>

        <Tooltip title={'Project'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Contents}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.FileSkewed />}
              onClick={onClickContentsTab}
            />
          </span>
        </Tooltip>

        <Tooltip title={'StoryBoards'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Settings}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Settings />}
              onClick={onClickSettingsTab}
            />
          </span>
        </Tooltip>

        <Tooltip title={'Dependencies'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Sharing}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.TwoGhosts />}
              onClick={onClickSharingTab}
            />
          </span>
        </Tooltip>

        <Tooltip title={'Sharing'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Github}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Octocat />}
              onClick={onClickGithubTab}
            />
          </span>
        </Tooltip>

        <Tooltip title={'Settings'} placement={'right'}>
          <span>
            <MenuTile
              selected={isPreviewPaneVisible}
              menuExpanded={false}
              icon={<MenuIcons.Settings />}
              onClick={togglePreviewPaneVisible}
              style={{ cursor: 'pointer' }}
            />
          </span>
        </Tooltip>
      </FlexColumn>
      {isFeatureEnabled('Performance Test Triggers') ? (
        <React.Fragment>
          <Tile style={{ marginTop: 12, marginBottom: 12 }}>
            <a onClick={onTriggerScrollTest}>P S</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }}>
            <a onClick={onTriggerResizeTest}>P R</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }}>
            <a onClick={onTriggerSelectionTest}>P E</a>
          </Tile>
        </React.Fragment>
      ) : null}
      {isFeatureEnabled('Re-parse Project Button') ? (
        <Tile style={{ marginTop: 12, marginBottom: 12 }}>
          <a onClick={onReparseClick}>R</a>
        </Tile>
      ) : null}
      <Tile style={{ marginTop: 12, marginBottom: 12 }}>
        <a href='/projects'>
          <Avatar loginState={userState.loginState} size={28} />
        </a>
      </Tile>
    </FlexColumn>
  )
})
Menubar.displayName = 'Menubar'
