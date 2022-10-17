/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import {
  useTriggerScrollPerformanceTest,
  useTriggerResizePerformanceTest,
  useTriggerSelectionPerformanceTest,
  useTriggerRegularHighlightPerformanceTest,
  useTriggerAllElementsHighlightPerformanceTest,
  useTriggerAbsoluteMoveLargePerformanceTest,
  useTriggerAbsoluteMoveSmallPerformanceTest,
  useTriggerSelectionChangePerformanceTest,
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
  useColorTheme,
} from '../../uuiui'
import { User } from '../../uuiui-deps'
import { EditorAction } from '../editor/action-types'
import { setLeftMenuTab, setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
import { LeftMenuTab } from '../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'

interface TileProps {
  size: keyof typeof UtopiaTheme.layout.rowHeight
}

const Tile = styled.div<TileProps>((props) => ({
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
  width: props.size,
}))

export interface MenuTileProps extends React.HTMLAttributes<HTMLDivElement>, TileProps {
  selected: boolean
  menuExpanded: boolean
  icon: React.ReactElement<IcnProps>
  size: keyof typeof UtopiaTheme.layout.rowHeight
}

export const MenuTile: React.FunctionComponent<React.PropsWithChildren<MenuTileProps>> = (
  props,
) => {
  const [hovered, setHovered] = React.useState(false)
  const colorTheme = useColorTheme()
  const handleOnMouseOver = React.useCallback(() => setHovered(true), [])
  const handleOnMouseOut = React.useCallback(() => setHovered(false), [])
  var foregroundColor: IcnProps['color'] = 'secondary'

  return (
    <Tile
      size={props.size}
      css={{
        height: 44,
        transition: 'all .1s ease-in-out',
        borderLeft:
          props.menuExpanded && props.selected
            ? `2px solid ${colorTheme.primary}`
            : '2px solid transparent',

        cursor: 'pointer',
        '& > *': {
          opacity: props.selected ? 1 : 0.5,
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
          color: props.selected ? 'primary' : 'secondary',
        })}
      </div>
    </Tile>
  )
}

function useRequestVSCodeStatus(): () => void {
  const vscodeState = useEditorState(
    (store) => ({
      vscodeReady: store.editor.vscodeReady,
      loadingScreenVisible: store.editor.vscodeLoadingScreenVisible,
    }),
    'useRequestVSCodeStatus',
  )

  return React.useCallback(
    // eslint-disable-next-line no-console
    () => console.log(`VSCode State: ${JSON.stringify(vscodeState)}`),
    [vscodeState],
  )
}

export const Menubar = React.memo(() => {
  const {
    dispatch,
    selectedTab,
    userState,
    leftMenuExpanded,
    projectId,
    projectName,
    isPreviewPaneVisible,
    isCanvasVisible,
    isCodeEditorVisible,
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      selectedTab: store.editor.leftMenu.selectedTab,
      userState: store.userState,
      leftMenuExpanded: store.editor.leftMenu.expanded,
      projectId: store.editor.id,
      projectName: store.editor.projectName,
      isPreviewPaneVisible: store.editor.preview.visible,
      isCanvasVisible: store.editor.canvas.visible,
      isCodeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
    }
  }, 'Menubar')

  const colorTheme = useColorTheme()

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

  const toggleCanvasVisible = React.useCallback(
    () => dispatch([setPanelVisibility('canvas', !isCanvasVisible)]),
    [dispatch, isCanvasVisible],
  )

  const toggleCodeEditorVisible = React.useCallback(
    () => dispatch([setPanelVisibility('codeEditor', !isCodeEditorVisible)]),
    [dispatch, isCodeEditorVisible],
  )

  const onReparseClick = useReParseOpenProjectFile()

  const onTriggerScrollTest = useTriggerScrollPerformanceTest()
  const onTriggerResizeTest = useTriggerResizePerformanceTest()
  const onTriggerRegularHighlightTest = useTriggerRegularHighlightPerformanceTest()
  const onTriggerAllElementsHighlightTest = useTriggerAllElementsHighlightPerformanceTest()
  const onTriggerSelectionTest = useTriggerSelectionPerformanceTest()
  const onTriggerAbsoluteMoveLargeTest = useTriggerAbsoluteMoveLargePerformanceTest()
  const onTriggerAbsoluteMoveSmallTest = useTriggerAbsoluteMoveSmallPerformanceTest()
  const onTriggerSelectionChangeTest = useTriggerSelectionChangePerformanceTest()

  const onRequestVSCodeStatus = useRequestVSCodeStatus()

  const previewURL =
    projectId == null ? '' : shareURLForProject(FLOATING_PREVIEW_BASE_URL, projectId, projectName)

  const entireStateRef = useRefEditorState((store) => store)

  const jsxMetadata = useRefEditorState((store) => {
    return store.editor.jsxMetadata
  })

  const printEditorState = React.useCallback(() => {
    console.info('Current Editor State:', entireStateRef.current)
    console.info('Latest metadata:', jsxMetadata.current)
  }, [entireStateRef, jsxMetadata])

  return (
    <FlexColumn
      id='leftMenuBar'
      style={{
        flexGrow: 1,
        backgroundColor: colorTheme.leftMenuBackground.value,
        width: 44,
        overflowX: 'scroll',
      }}
    >
      <FlexColumn style={{ flexGrow: 1 }}>
        <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
          <a href='/projects'>
            <Avatar
              isLoggedIn={User.isLoggedIn(userState.loginState)}
              userPicture={User.getUserPicture(userState.loginState)}
              size={28}
            />
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

        <Tooltip title={'Show or hide the code editor'} placement={'left'}>
          <span>
            <MenuTile
              selected={isCodeEditorVisible}
              menuExpanded={false}
              icon={<LargerIcons.Code />}
              onClick={toggleCodeEditorVisible}
              size='large'
            />
          </span>
        </Tooltip>
        <Tooltip title={'Show or hide the canvas'} placement={'right'}>
          <span>
            <MenuTile
              selected={isCanvasVisible}
              menuExpanded={false}
              icon={<LargerIcons.DesignTool />}
              onClick={toggleCanvasVisible}
              size='large'
            />
          </span>
        </Tooltip>
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
            <a onClick={printEditorState}>PPP</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerScrollTest}>P S</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerResizeTest}>P R</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerRegularHighlightTest}>PRH</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerAllElementsHighlightTest}>PAH</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerSelectionTest}>P E</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerAbsoluteMoveLargeTest}>PAML</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerAbsoluteMoveSmallTest}>PAMS</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onTriggerSelectionChangeTest}>PSC</a>
          </Tile>
          <Tile style={{ marginTop: 12, marginBottom: 12 }} size='large'>
            <a onClick={onRequestVSCodeStatus}>VSC</a>
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
