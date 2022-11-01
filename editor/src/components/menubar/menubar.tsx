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
import { createSelector } from 'reselect'
import { getGithubFileChangesCount, githubFileChangesSelector } from '../../core/shared/github'

interface TileProps {
  size: keyof typeof UtopiaTheme.layout.rowHeight
}

const GITHUB_FILE_CHANGES_BADGE_LIMIT = 99

const Tile = styled.div<TileProps>((props) => ({
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
  width: props.size,
}))

export interface MenuTileProps extends React.HTMLAttributes<HTMLDivElement>, TileProps {
  selected: boolean
  icon: React.ReactElement<IcnProps>
  size: keyof typeof UtopiaTheme.layout.rowHeight
  badge?: string
}

export const MenuTile: React.FunctionComponent<React.PropsWithChildren<MenuTileProps>> = (
  props,
) => {
  const colorTheme = useColorTheme()

  return (
    <Tile
      size={props.size}
      css={{
        height: 44,
        transition: 'all .1s ease-in-out',
        borderLeft: props.selected ? `2px solid ${colorTheme.primary}` : '2px solid transparent',

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
          position: 'relative',
        }}
      >
        {React.cloneElement(props.icon, {
          color: props.selected ? 'primary' : 'secondary',
        })}
        {props.badge && <MenuTileBadge text={props.badge} />}
      </div>
    </Tile>
  )
}

const MenuTileBadge = ({ text }: { text: string }) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        position: 'absolute',
        top: 0,
        right: 0,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: colorTheme.contextMenuHighlightBackground.value,
        minWidth: 13,
        height: 13,
        paddingLeft: 3,
        paddingRight: 3,
        borderRadius: '10px',
        fontSize: 7,
        fontWeight: 800,
        color: '#fff',
      }}
    >
      {text}
    </div>
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

const githubFileChangesCountSelector = createSelector(githubFileChangesSelector, (changes) => {
  return getGithubFileChangesCount(changes)
})

export const Menubar = React.memo(() => {
  const { dispatch, selectedTab, projectId, projectName } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      selectedTab: store.editor.leftMenu.selectedTab,
      projectId: store.editor.id,
      projectName: store.editor.projectName,
    }
  }, 'Menubar')

  const colorTheme = useColorTheme()

  const onClickTab = React.useCallback(
    (menuTab: LeftMenuTab) => {
      let actions: Array<EditorAction> = []
      actions.push(setLeftMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch],
  )

  const onClickContentsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Contents)
  }, [onClickTab])

  const onClickSettingsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Settings)
  }, [onClickTab])

  const onClickGithubTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Github)
  }, [onClickTab])

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

  const githubFileChangesCount = useEditorState(
    githubFileChangesCountSelector,
    'Github file changes count',
  )

  const githubFileChangesCountLabel = React.useMemo(() => {
    if (githubFileChangesCount <= 0) {
      return undefined
    }
    if (githubFileChangesCount < GITHUB_FILE_CHANGES_BADGE_LIMIT) {
      return `${githubFileChangesCount}`
    }
    return `${GITHUB_FILE_CHANGES_BADGE_LIMIT}+`
  }, [githubFileChangesCount])

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
        <Tooltip title={'Files, Dependencies, Fonts'} placement={'right'}>
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Contents}
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
              icon={<MenuIcons.Settings />}
              onClick={onClickSettingsTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip
          title={
            githubFileChangesCount > 0
              ? `Github (${githubFileChangesCount} file${
                  githubFileChangesCount !== 1 ? 's' : ''
                } changed)`
              : 'Github'
          }
          placement={'right'}
        >
          <span>
            <MenuTile
              selected={selectedTab === LeftMenuTab.Github}
              icon={<MenuIcons.Octocat />}
              onClick={onClickGithubTab}
              size='large'
              badge={githubFileChangesCountLabel}
            />
          </span>
        </Tooltip>

        <a style={{ marginTop: 32 }} target='_blank' rel='noopener noreferrer' href={previewURL}>
          <Tooltip title={'Launch External Preview'} placement={'right'}>
            <span>
              <MenuTile selected={false} icon={<MenuIcons.ExternalLink />} size='large' />
            </span>
          </Tooltip>
        </a>
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
