import React, { useCallback, useState } from 'react'
import { createSelector } from 'reselect'
import { auth0Url } from '../../common/env-vars'
import { getUserPicture } from '../../common/user'
import { getGithubFileChangesCount, useGithubFileChanges } from '../../core/shared/github/helpers'
import { unless, when } from '../../utils/react-conditionals'
import {
  Avatar,
  Button,
  colorTheme,
  Icons,
  LargerIcons,
  SimpleFlexRow,
  UNSAFE_getIconURL,
  useColorTheme,
  UtopiaTheme,
} from '../../uuiui'
import type { LoginState } from '../../uuiui-deps'
import type { EditorAction } from '../editor/action-types'
import {
  openCodeEditorFile,
  setLeftMenuTab,
  setPanelVisibility,
  togglePanel,
} from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import type { EditorStoreShared } from '../editor/store/editor-state'
import { EditorStorePatched, githubRepoFullName, LeftMenuTab } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { RoundButton } from './buttons'
import { TestMenu } from './test-menu'
import { useGridPanelDraggable } from '../canvas/grid-panels-dnd'
import { FlexRow } from 'utopia-api'
import {
  useUpdateGridPanelLayout,
  useUpdateGridPanelLayoutPutCodeEditorBelowNavigator,
} from '../canvas/grid-panels-state'
import { NO_OP } from '../../core/shared/utils'
import type { StoredPanel } from '../canvas/stored-layout'
import { UserBar } from '../user-bar'

interface ProjectTitleProps {}

export const TitleHeight = 40

const ProjectTitle: React.FC<React.PropsWithChildren<ProjectTitleProps>> = ({ children }) => {
  return (
    <span
      style={{
        fontWeight: 400,
        fontSize: 12,
        padding: '0 10px',
        color: colorTheme.fg0.value,
        height: TitleHeight,
        alignItems: 'center',
        overflow: 'hidden',
        whiteSpace: 'nowrap',
        textOverflow: 'ellipsis',
      }}
    >
      {children}
    </span>
  )
}

type PanelButtonProps = {
  onClick?: () => void
  color?: string
  isHovered?: boolean
  children?: React.ReactNode
}

export const PanelButton = (props: PanelButtonProps) => {
  return (
    <div
      onClick={props.onClick !== null ? props.onClick : NO_OP}
      style={{
        width: 8,
        height: 8,
        borderRadius: 8,
        pointerEvents: 'initial',
        backgroundColor:
          props.isHovered && props.color !== null ? props.color : colorTheme.unavailableGrey.value,
      }}
    >
      {props.children}
    </div>
  )
}

export const TitleBarProjectTitle = React.memo((props: { panelData: StoredPanel }) => {
  const { drag } = useGridPanelDraggable(props.panelData)

  const dispatch = useDispatch()
  const theme = useColorTheme()

  const { upstreamChanges, currentBranch, treeConflicts } = useEditorState(
    Substores.github,
    (store) => {
      return {
        upstreamChanges: store.editor.githubData.upstreamChanges,
        currentBranch: store.editor.githubSettings.branchName,
        treeConflicts: store.editor.githubData.treeConflicts,
      }
    },
    'TitleBar github',
  )

  const openFile = React.useCallback(
    (filename: string) => {
      dispatch([openCodeEditorFile(filename, true)], 'everyone')
    },
    [dispatch],
  )
  const showMergeConflict = React.useCallback(() => {
    if (Object.keys(treeConflicts).length < 1) {
      return
    }
    const firstConflictFilename = Object.keys(treeConflicts)[0]
    openFile(firstConflictFilename)
  }, [openFile, treeConflicts])

  const hasUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges) > 0,
    [upstreamChanges],
  )

  const hasMergeConflicts = React.useMemo(() => {
    if (treeConflicts == null) {
      return false
    }
    return Object.keys(treeConflicts).length > 0
  }, [treeConflicts])

  const githubFileChanges = useGithubFileChanges()
  const hasDownstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(githubFileChanges) > 0,
    [githubFileChanges],
  )
  const openLeftPaneltoGithubTab = useCallback(() => {
    dispatch([setPanelVisibility('leftmenu', true), setLeftMenuTab(LeftMenuTab.Github)])
  }, [dispatch])

  const { loginState } = useEditorState(
    Substores.restOfStore,
    (store) => ({
      loginState: store.userState.loginState,
    }),
    'TitleBar loginState',
  )

  const loggedIn = React.useMemo(() => loginState.type === 'LOGGED_IN', [loginState])

  const onMouseDown = useCallback((e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
  }, [])

  const toggleNavigatorVisible = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])
  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  const projectName = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.projectName
    },
    'TitleBar projectName',
  )

  return (
    <div
      ref={drag}
      className='handle'
      style={{
        height: TitleHeight,
        width: '100%',
        backgroundColor: theme.inspectorBackground.value,
        padding: '0 10px',
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        justifyContent: 'space-between',
        gap: 10,
        flexShrink: 0,
      }}
      onMouseEnter={setIsHoveredTrue}
      onMouseLeave={setIsHoveredFalse}
    >
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '22px 1fr auto',
          width: '100%',
          alignItems: 'center',
          gap: 4,
        }}
      >
        <FlexRow css={{ gap: 6 }}>
          <PanelButton onClick={toggleNavigatorVisible} color='#FF5F57' isHovered={isHovered} />
          <PanelButton isHovered={isHovered} color={colorTheme.unavailableGrey.value} />
        </FlexRow>
        <span style={{ overflow: 'hidden', textOverflow: 'ellipsis' }}>
          {currentBranch != null ? (
            <SimpleFlexRow
              onClick={showMergeConflict}
              style={{
                gap: 4,
                color: hasMergeConflicts ? colorTheme.error.value : colorTheme.fg1.value,
              }}
            >
              {hasMergeConflicts ? (
                <Icons.WarningTriangle style={{ width: 18, height: 18 }} color={'error'} />
              ) : (
                <Icons.Branch style={{ width: 18, height: 18 }} />
              )}
              {currentBranch}
            </SimpleFlexRow>
          ) : (
            <ProjectTitle>{projectName}</ProjectTitle>
          )}
        </span>
        <span>
          {when(
            loggedIn && currentBranch != null,
            <FlexRow
              css={{
                gap: 2,
              }}
            >
              {when(
                hasUpstreamChanges,
                <RoundButton
                  color={colorTheme.secondaryOrange.value}
                  onClick={openLeftPaneltoGithubTab}
                  onMouseDown={onMouseDown}
                >
                  {<Icons.Download style={{ width: 18, height: 18 }} color={'component-orange'} />}
                </RoundButton>,
              )}
              {when(
                hasDownstreamChanges,
                <RoundButton
                  color={colorTheme.secondaryBlue.value}
                  onClick={openLeftPaneltoGithubTab}
                  onMouseDown={onMouseDown}
                >
                  {<Icons.Upload style={{ width: 18, height: 18 }} color={'dynamic'} />}
                </RoundButton>,
              )}
            </FlexRow>,
          )}
        </span>
      </div>
    </div>
  )
})

export const onClickSignIn = () => {
  window.open(auth0Url('auto-close'), '_blank')
}

export const TitleBarUserProfile = React.memo((props: { panelData: StoredPanel }) => {
  const { drag } = useGridPanelDraggable(props.panelData)
  const dispatch = useDispatch()

  const theme = useColorTheme()
  const { loginState } = useEditorState(
    Substores.restOfStore,
    (store) => ({
      loginState: store.userState.loginState,
    }),
    'TitleBar loginState',
  )

  const loggedIn = React.useMemo(() => loginState.type === 'LOGGED_IN', [loginState])

  const onMouseDown = useCallback((e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
  }, [])

  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([togglePanel('rightmenu')])
  }, [dispatch])

  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])
  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  return (
    <div
      ref={drag}
      className='handle'
      style={{
        height: TitleHeight,
        width: '100%',
        backgroundColor: theme.inspectorBackground.value,
        padding: '0 8px',
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        justifyContent: 'space-between',
        gap: 6,
        flexShrink: 0,
      }}
      onMouseEnter={setIsHoveredTrue}
      onMouseLeave={setIsHoveredFalse}
    >
      <FlexRow css={{ gap: 6 }}>
        <PanelButton onClick={toggleInspectorVisible} color='#FF5F57' isHovered={isHovered} />
        <PanelButton isHovered={isHovered} color={colorTheme.unavailableGrey.value} />
      </FlexRow>
      <div style={{ flex: '0 0 0px' }} data-testid='sign-in-button'>
        {unless(
          loggedIn,
          <Button
            highlight
            style={{
              paddingLeft: 8,
              paddingRight: 8,
              background: colorTheme.primary.value,
              color: colorTheme.white.value,
              fontWeight: 600,
              borderRadius: 30,
            }}
            onClick={onClickSignIn}
            onMouseDown={onMouseDown}
          >
            Sign In
          </Button>,
        )}
        <UserBar />
      </div>
    </div>
  )
})

export const TitleBarEmpty = React.memo((props: { panelData: StoredPanel }) => {
  const { drag } = useGridPanelDraggable(props.panelData)
  const theme = useColorTheme()
  return (
    <div
      ref={drag}
      className='handle'
      style={{
        height: TitleHeight,
        width: '100%',
        backgroundColor: theme.inspectorBackground.value,
        paddingLeft: 10,
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        gap: 6,
      }}
    >
      <PanelButton color={colorTheme.unavailableGrey.value} />
      <PanelButton color={colorTheme.unavailableGrey.value} />
    </div>
  )
})

export const TitleBarCode = React.memo((props: { panelData: StoredPanel }) => {
  const { drag } = useGridPanelDraggable(props.panelData)
  const dispatch = useDispatch()
  const theme = useColorTheme()

  const updatePanelLayout = useUpdateGridPanelLayout()
  const onMaximize = React.useCallback(() => {
    updatePanelLayout('code-editor', { type: 'before-column', columnIndex: 0 })
  }, [updatePanelLayout])

  const onMinimize = useUpdateGridPanelLayoutPutCodeEditorBelowNavigator()

  const toggleCodeEditorVisible = React.useCallback(
    () => dispatch([togglePanel('codeEditor')]),
    [dispatch],
  )

  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])
  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  const { currentBranch, repoName } = useEditorState(
    Substores.github,
    (store) => {
      return {
        currentBranch: store.editor.githubSettings.branchName,
        repoName: githubRepoFullName(store.editor.githubSettings.targetRepository),
      }
    },
    'TitleBar github',
  )

  return (
    <div
      ref={drag}
      className='handle'
      style={{
        height: 40,
        flexShrink: 0,
        width: '100%',
        backgroundColor: theme.inspectorBackground.value,
        padding: '0 10px',
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        gap: 10,
      }}
      onMouseEnter={setIsHoveredTrue}
      onMouseLeave={setIsHoveredFalse}
    >
      <FlexRow css={{ gap: 6 }}>
        <PanelButton onClick={toggleCodeEditorVisible} color='#FF5F57' isHovered={isHovered} />
        <PanelButton onClick={onMinimize} color='#FDBC40' isHovered={isHovered} />
        <PanelButton onClick={onMaximize} color='#33C748' isHovered={isHovered} />
      </FlexRow>

      {currentBranch != null ? (
        <SimpleFlexRow>{repoName}</SimpleFlexRow>
      ) : (
        <span style={{ fontWeight: 600 }}>Code</span>
      )}
    </div>
  )
})
