import React, { useCallback } from 'react'
import { createSelector } from 'reselect'
import { secondaryErrorStyle } from 'src/third-party/react-error-overlay/styles'
import { auth0Url } from '../../common/env-vars'
import { getUserPicture } from '../../common/user'
import { getGithubFileChangesCount, useGithubFileChanges } from '../../core/shared/github'
import { unless, when } from '../../utils/react-conditionals'
import {
  Avatar,
  Button,
  colorTheme,
  Icons,
  LargerIcons,
  SimpleFlexRow,
  UNSAFE_getIconURL,
} from '../../uuiui'
import { LoginState } from '../../uuiui-deps'
import { EditorAction } from '../editor/action-types'
import { setLeftMenuTab, setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
import { EditorStorePatched, githubRepoFullName, LeftMenuTab } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { RoundButton } from './buttons'
import { TestMenu } from './test-menu'

interface ProjectTitleProps {}

const ProjectTitle: React.FC<React.PropsWithChildren<ProjectTitleProps>> = ({ children }) => {
  return (
    <div
      style={{
        fontWeight: 400,
        fontSize: 12,
        padding: '0 10px',
        color: colorTheme.fg0.value,
      }}
    >
      {children}
    </div>
  )
}

const TitleBar = React.memo(() => {
  const { dispatch, loginState, projectName, upstreamChanges, currentBranch } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      loginState: store.userState.loginState,
      projectName: store.editor.projectName,
      upstreamChanges: store.editor.githubData.upstreamChanges,
      currentBranch: store.editor.githubSettings.branchName,
    }),
    'TitleBar',
  )

  const userPicture = useGetUserPicture()

  const repoName = useEditorState(
    (store) => githubRepoFullName(store.editor.githubSettings.targetRepository),
    'RepositoryBlock repo',
  )

  const hasUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges) > 0,
    [upstreamChanges],
  )
  const numberOfUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges),
    [upstreamChanges],
  )

  const githubFileChanges = useGithubFileChanges()
  const hasDownstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(githubFileChanges) > 0,
    [githubFileChanges],
  )
  const numberOfDownstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(githubFileChanges),
    [githubFileChanges],
  )

  const onClickLoginNewTab = useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  const isLeftMenuExpanded = useEditorState(
    (store) => store.editor.leftMenu.expanded,
    'LeftPanelRoot isLeftMenuExpanded',
  )

  const toggleLeftPanel = useCallback(() => {
    let actions: Array<EditorAction> = []
    actions.push(togglePanel('leftmenu'))
    dispatch(actions)
  }, [dispatch])

  const openLeftPanel = useCallback(() => {
    // {
    //   isLeftMenuExpanded ? console.log('highlight the CTA') : null
    // }
    let actions: Array<EditorAction> = []
    actions.push(setPanelVisibility('leftmenu', true))
    actions.push(setLeftMenuTab(LeftMenuTab.Github))
    dispatch(actions)
  }, [dispatch])

  const loggedIn = React.useMemo(() => loginState.type === 'LOGGED_IN', [loginState])

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
              <RoundButton color={colorTheme.secondaryOrange.value} onClick={openLeftPanel}>
                {<Icons.Download style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                <>Pull Remote</>
              </RoundButton>,
            )}
            {when(
              hasDownstreamChanges,
              <RoundButton color={colorTheme.secondaryBlue.value} onClick={openLeftPanel}>
                {<Icons.Upload style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                <>Push Local</>
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
          height: 27,
        }}
      >
        {currentBranch ? (
          <SimpleFlexRow style={{ gap: 5 }}>
            {repoName}
            {<Icons.Branch style={{ width: 19, height: 19 }} />}
            {currentBranch}
          </SimpleFlexRow>
        ) : (
          <ProjectTitle>{projectName}</ProjectTitle>
        )}
      </SimpleFlexRow>
      <div style={{ flexGrow: 1 }} />
      <div style={{ flex: '0 0 0px', paddingRight: 8 }}>
        {unless(
          loggedIn,
          <Button primary style={{ paddingLeft: 8, paddingRight: 8 }} onClick={onClickLoginNewTab}>
            Sign In To Save
          </Button>,
        )}
        {when(loggedIn, <Avatar userPicture={userPicture} isLoggedIn={loggedIn} />)}
      </div>
    </SimpleFlexRow>
  )
})

export default TitleBar

const loginStateSelector = createSelector(
  (store: EditorStorePatched) => store.userState.loginState,
  (loginState: LoginState) => getUserPicture(loginState),
)
function useGetUserPicture(): string | null {
  return useEditorState(loginStateSelector, 'useGetUserPicture')
}
