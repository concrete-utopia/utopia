import React, { useCallback } from 'react'
import { jsx, css } from '@emotion/react'
import { auth0Url } from '../../common/env-vars'
import { getGithubFileChangesCount, githubFileChangesSelector } from '../../core/shared/github'
import { unless, when } from '../../utils/react-conditionals'
import {
  Avatar,
  Button,
  colorTheme,
  LargerIcons,
  MenuIcons,
  SimpleFlexRow,
  UtopiaTheme,
} from '../../uuiui'
import { EditorAction } from '../editor/action-types'
import { setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
import { useEditorState } from '../editor/store/store-hook'
import { RoundButton, SquareButton } from './buttons'
import { MenuTile } from './menu-tile'
import { TestMenu } from './test-menu'
import { useGetProjectMetadata } from '../common/server-hooks'

interface ProjectTitleProps {}

const ProjectTitle: React.FC<React.PropsWithChildren<ProjectTitleProps>> = ({ children }) => {
  return (
    <div style={{ fontWeight: 400, fontSize: 12, padding: '0 10px', color: colorTheme.fg0.value }}>
      {children}
    </div>
  )
}

const TitleBar = React.memo(() => {
  const {
    dispatch,
    loginState,
    projectName,
    isCodeEditorVisible,
    isPreviewPaneVisible,
    upstreamChanges,
    targetRepository,
    id,
  } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      loginState: store.userState.loginState,
      projectName: store.editor.projectName,
      isCodeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
      isPreviewPaneVisible: store.editor.preview.visible,
      upstreamChanges: store.editor.githubData.upstreamChanges,
      targetRepository: store.editor.githubSettings.targetRepository,
      id: store.editor.id,
    }),
    'TitleBar',
  )
  const projectOwnerMetadata = useGetProjectMetadata(id)
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

  const toggleCodeEditorVisible = React.useCallback(
    () => dispatch([setPanelVisibility('codeEditor', !isCodeEditorVisible)]),
    [dispatch, isCodeEditorVisible],
  )

  const togglePreviewPaneVisible = React.useCallback(
    () => dispatch([setPanelVisibility('preview', !isPreviewPaneVisible)]),
    [dispatch, isPreviewPaneVisible],
  )

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
                {numberOfUpstreamChanges}
                <> Remote Change{numberOfUpstreamChanges !== 1 ? 's' : ''}</>
              </RoundButton>,
            )}
            {when(
              hasDownstreamChanges,
              <RoundButton color={colorTheme.secondaryBlue.value} onClick={toggleLeftPanel}>
                {numberOfDownstreamChanges}
                <> Local Change{numberOfDownstreamChanges !== 1 ? 's' : ''}</>
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
          background: '#fafafa',
          height: 31,
        }}
      >
        <ProjectTitle>{projectName}</ProjectTitle>
      </SimpleFlexRow>
      <SimpleFlexRow style={{ display: 'flex', height: '100%', flexGrow: 1 }}>
        <TestMenu />
      </SimpleFlexRow>
      <div style={{ flex: '0 0 0px', paddingRight: 8 }}>
        {unless(
          loggedIn,
          <Button primary style={{ paddingLeft: 8, paddingRight: 8 }} onClick={onClickLoginNewTab}>
            Sign In To Save
          </Button>,
        )}
        {when(
          loggedIn,
          <Avatar userPicture={projectOwnerMetadata?.ownerPicture ?? null} isLoggedIn={loggedIn} />,
        )}
      </div>
    </SimpleFlexRow>
  )
})

export default TitleBar
