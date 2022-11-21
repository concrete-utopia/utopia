import React, { useCallback } from 'react'
import { createSelector } from 'reselect'
import { auth0Url } from '../../common/env-vars'
import { getUserPicture } from '../../common/user'
import { getGithubFileChangesCount, githubFileChangesSelector } from '../../core/shared/github'
import { unless, when } from '../../utils/react-conditionals'
import { Avatar, Button, colorTheme, SimpleFlexRow } from '../../uuiui'
import { LoginState } from '../../uuiui-deps'
import { EditorAction } from '../editor/action-types'
import { togglePanel } from '../editor/actions/action-creators'
import { EditorStorePatched } from '../editor/store/editor-state'
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
  const { dispatch, loginState, projectName, upstreamChanges } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      loginState: store.userState.loginState,
      projectName: store.editor.projectName,
      upstreamChanges: store.editor.githubData.upstreamChanges,
    }),
    'TitleBar',
  )

  const userPicture = useGetUserPicture()

  const hasUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges) > 0,
    [upstreamChanges],
  )
  const numberOfUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges),
    [upstreamChanges],
  )

  const githubFileChanges = useEditorState(githubFileChangesSelector, 'Github file changes')
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

  const toggleLeftPanel = useCallback(() => {
    let actions: Array<EditorAction> = []
    actions.push(togglePanel('leftmenu'))
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
          <img src='/editor/pyramid_dark@2x.png' width='24' alt='Main Menu' />
        </RoundButton>
        {when(
          loggedIn,
          <>
            {when(
              hasUpstreamChanges,
              <RoundButton color={colorTheme.secondaryOrange.value} onClick={toggleLeftPanel}>
                {/* {numberOfUpstreamChanges} */}
                {/* {numberOfUpstreamChanges !== 1 ? 's' : ''} */}
                <>Remote Changes</>
              </RoundButton>,
            )}
            {when(
              hasDownstreamChanges,
              <RoundButton color={colorTheme.secondaryBlue.value} onClick={toggleLeftPanel}>
                {/* {numberOfDownstreamChanges} */}
                {/* {numberOfDownstreamChanges !== 1 ? 's' : ''} */}
                <>Local Changes</>
              </RoundButton>,
            )}
          </>,
        )}
      </SimpleFlexRow>

      <SimpleFlexRow
        style={{
          paddingLeft: 20,
          paddingRight: 20,
          borderRadius: 10,
          height: 30,
          background: colorTheme.fg9.value,
        }}
      >
        <ProjectTitle>{projectName}</ProjectTitle>
      </SimpleFlexRow>
      <SimpleFlexRow style={{ display: 'flex', height: '100%', flexGrow: 1, marginLeft: 10 }}>
        <TestMenu />
      </SimpleFlexRow>
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
