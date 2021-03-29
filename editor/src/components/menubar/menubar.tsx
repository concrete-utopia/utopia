/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import * as React from 'react'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
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
  FlexColumn,
  Tooltip,
  MenuIcons,
  LargerIcons,
  Avatar,
  UtopiaTheme,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { EditorAction } from '../editor/action-types'
import { setLeftMenuTab, setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
import { useEditorState } from '../editor/store/store-hook'
import { LeftMenuTab } from '../navigator/left-pane'

interface TileProp {
  size: keyof typeof UtopiaTheme.layout.rowHeight
}

const Tile = styled.div<TileProp>((props) => ({
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
  width: props.size,
}))

export interface MenuTileProps extends React.HTMLAttributes<HTMLDivElement> {
  selected: boolean
  menuExpanded: boolean
  icon: React.ReactElement<IcnProps>
  size: keyof typeof UtopiaTheme.layout.rowHeight
}

export const MenuTile: React.FunctionComponent<MenuTileProps> = (props) => {
  const [hovered, setHovered] = React.useState(false)

  const handleOnMouseOver = React.useCallback(() => setHovered(true), [])
  const handleOnMouseOut = React.useCallback(() => setHovered(false), [])
  var foregroundColor: IcnProps['color'] = 'darkgray'

  return (
    <Tile
      size={props.size}
      css={{
        height: 44,
        transition: 'all .1s ease-in-out',
        borderLeft:
          props.menuExpanded && props.selected ? '2px solid darkgray' : '1px solid transparent',
        cursor: 'pointer',
        '& > *': {
          opacity: props.selected ? 1 : 0.33,
          transform: props.selected ? 'translateX(1px)' : 'inherit',
        },
        '&:hover > *': {
          opacity: 1,
          transform: props.selected ? 'translateX(1px)' : 'inherit',
        },
        '&:active > *': {
          transform: 'translateX(1px)',
          opacity: 1,
        },
      }}
      onMouseOver={handleOnMouseOver}
      onMouseOut={handleOnMouseOut}
      onClick={props.onClick}
    >
      <div
        style={{
          ...props.style,
          borderRadius: 1,
          width: 28,
          height: 28,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        {React.cloneElement(props.icon, {
          color: foregroundColor,
        })}
      </div>
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
        backgroundColor: UtopiaTheme.color.leftMenuBackground.value,
      }}
    >
      <FlexColumn style={{ flexGrow: 1 }}>
        <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
          <a href='/projects'>
            <Avatar loginState={userState.loginState} size={28} />
          </a>
        </Tile>

        <Tooltip title={'Project Info'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Project}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Smiangle />}
              onClick={onClickProjectTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Storyboards'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Storyboards}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Pyramid />}
              onClick={onClickStoryboardsTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Files, Dependencies, Fonts'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Contents}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.FileSkewed />}
              onClick={onClickContentsTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Settings'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Settings}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Settings />}
              onClick={onClickSettingsTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Sharing'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Sharing}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.TwoGhosts />}
              onClick={onClickSharingTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Github'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Github}
              menuExpanded={leftMenuExpanded}
              icon={<MenuIcons.Octocat />}
              onClick={onClickGithubTab}
              size='large'
            />
          </span>
        </Tooltip>

        <a style={{ marginTop: 32 }} target='_blank' rel='noopener noreferrer' href={previewURL}>
          <Tooltip title={'Launch External Preview'} placement={'right'}>
            <span>
              <MenuTile
                selected={false}
                menuExpanded={false}
                icon={<MenuIcons.ExternalLink />}
                size='large'
              />
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
              size='large'
            />
          </span>
        </Tooltip>
      </FlexColumn>
      {isFeatureEnabled('Performance Test Triggers') ? (
        <React.Fragment>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerScrollTest}>P S</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerResizeTest}>P R</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerSelectionTest}>P E</a>
          </Tile>
        </React.Fragment>
      ) : null}
      {isFeatureEnabled('Re-parse Project Button') ? (
        <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
          <a onClick={onReparseClick}>R</a>
        </Tile>
      ) : null}
    </FlexColumn>
  )
})
Menubar.displayName = 'Menubar'
