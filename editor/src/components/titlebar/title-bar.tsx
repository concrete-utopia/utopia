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
  SquareButton,
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
import { SinglePlayerUserBar, UserBar } from '../user-bar'

interface ProjectTitleProps {}

export const TitleHeight = 40

const ProjectTitle: React.FC<React.PropsWithChildren<ProjectTitleProps>> = ({ children }) => {
  return (
    <FlexRow
      style={{
        fontWeight: 400,
        fontSize: 12,
        padding: '0 10px',
        color: colorTheme.fg0.value,
        height: TitleHeight,
        alignItems: 'center',
      }}
      css={undefined}
    >
      {children}
    </FlexRow>
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
      <FlexRow css={{ gap: 10, alignItems: 'center' }}>
        <FlexRow css={{ gap: 6 }}>
          <PanelButton onClick={toggleNavigatorVisible} color='#FF5F57' isHovered={isHovered} />
          <PanelButton isHovered={isHovered} color={colorTheme.unavailableGrey.value} />
        </FlexRow>
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
      </FlexRow>
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
    </div>
  )
})

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
      <div style={{ flex: '0 0 0px' }}>
        {unless(loggedIn, <SignInButton text='save' />)}
        <UserBar />
      </div>
    </div>
  )
})

export const SignInButton = React.memo((props: { text: 'save' | 'load' }) => {
  const onClickLoginNewTab = useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  const onMouseDown = useCallback((e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
  }, [])

  return (
    <Button
      data-testid='sign-in-button'
      highlight
      style={{
        paddingLeft: 8,
        paddingRight: 8,
        background: colorTheme.dynamicBlue.value,
        color: colorTheme.bg1.value,
        fontWeight: 600,
      }}
      onClick={onClickLoginNewTab}
      onMouseDown={onMouseDown}
    >
      {props.text === 'save' ? 'Sign In To Save' : 'Sign In To Load'}
    </Button>
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

const TitleBar = React.memo(() => {
  const dispatch = useDispatch()
  const { loginState } = useEditorState(
    Substores.restOfStore,
    (store) => ({
      loginState: store.userState.loginState,
    }),
    'TitleBar loginState',
  )

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

  const onClickLoginNewTab = useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  const toggleLeftPanel = useCallback(() => {
    let actions: Array<EditorAction> = []
    actions.push(togglePanel('leftmenu'))
    dispatch(actions)
  }, [dispatch])

  const openLeftPaneltoGithubTab = useCallback(() => {
    dispatch([setPanelVisibility('leftmenu', true), setLeftMenuTab(LeftMenuTab.Github)])
  }, [dispatch])

  const loggedIn = React.useMemo(() => loginState.type === 'LOGGED_IN', [loginState])

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
    openLeftPaneltoGithubTab()
    openFile(firstConflictFilename)
  }, [openLeftPaneltoGithubTab, openFile, treeConflicts])

  return (
    <SimpleFlexRow
      style={{
        backgroundColor: colorTheme.bg0.value,
        borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
        padding: 0,
        flex: '0 0 40px',
        fontWeight: 600,
        letterSpacing: 0.2,
        justifyContent: 'space-between',
      }}
    >
      <SimpleFlexRow
        style={{
          display: 'flex',
          height: '100%',
          alignItems: 'center',
          flex: '1 1 0px',
          gap: 10,
          paddingLeft: 8,
        }}
      >
        <RoundButton onClick={toggleLeftPanel}>
          <img
            style={{
              userSelect: 'none',
              display: 'block',
            }}
            width={30}
            src={UNSAFE_getIconURL('utopia-logo', 'black', 'special', 60, 47)}
          />
        </RoundButton>
        <TestMenu />
        {when(
          loggedIn,
          <>
            {when(
              hasUpstreamChanges,
              <RoundButton
                color={colorTheme.secondaryOrange.value}
                onClick={openLeftPaneltoGithubTab}
              >
                {<Icons.Download style={{ width: 19, height: 19 }} color={'on-light-main'} />}
              </RoundButton>,
            )}
            {when(
              hasMergeConflicts,
              <RoundButton color={colorTheme.error.value} onClick={showMergeConflict}>
                {
                  <Icons.WarningTriangle
                    style={{ width: 19, height: 19 }}
                    color={'on-light-main'}
                  />
                }
              </RoundButton>,
            )}
            {when(
              hasDownstreamChanges,
              <RoundButton
                color={colorTheme.secondaryBlue.value}
                onClick={openLeftPaneltoGithubTab}
              >
                {<Icons.Upload style={{ width: 19, height: 19 }} color={'on-light-main'} />}
              </RoundButton>,
            )}
          </>,
        )}
      </SimpleFlexRow>
      <SimpleFlexRow
        style={{
          paddingLeft: 16,
          paddingRight: 16,
          borderRadius: 16,
          background: colorTheme.bg1.value,
        }}
      >
        {currentBranch != null ? (
          <SimpleFlexRow style={{ gap: 5 }}>
            {<Icons.Branch style={{ width: 19, height: 19 }} />}
            {currentBranch}
          </SimpleFlexRow>
        ) : null}
      </SimpleFlexRow>
      <div style={{ flexGrow: 1 }} />
      <div style={{ flex: '0 0 0px', paddingRight: 8 }}>
        {unless(
          loggedIn,
          <Button
            highlight
            style={{
              paddingLeft: 8,
              paddingRight: 8,
              background: colorTheme.dynamicBlue.value,
              color: colorTheme.bg1.value,
              fontWeight: 600,
              borderRadius: 20,
            }}
            onClick={onClickLoginNewTab}
          >
            Sign In To Save
          </Button>,
        )}
        <SinglePlayerUserBar />
      </div>
    </SimpleFlexRow>
  )
})

export default TitleBar
