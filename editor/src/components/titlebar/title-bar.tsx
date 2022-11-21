import React, { useCallback } from 'react'
import { auth0Url } from '../../common/env-vars'
import { colorTheme, LargerIcons, MenuIcons, SimpleFlexRow } from '../../uuiui'
import { EditorAction } from '../editor/action-types'
import { setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
import { useEditorState } from '../editor/store/store-hook'
import { MenuTile } from './menu-tile'
import { SquareButton, RoundButton } from './buttons'
import { TestMenu } from './test-menu'
import { getGithubFileChangesCount, githubFileChangesSelector } from '../../core/shared/github'
import { unless, when } from 'src/utils/react-conditionals'

const AppLogo: React.FC<{ onClick: () => void }> = ({ onClick }) => (
  <div
    onClick={onClick}
    style={{
      cursor: 'pointer',
    }}
  >
    <MenuIcons.Smiangle />
  </div>
)

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
  } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      loginState: store.userState.loginState,
      projectName: store.editor.projectName,
      isCodeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
      isPreviewPaneVisible: store.editor.preview.visible,
      upstreamChanges: store.editor.githubData.upstreamChanges,
      targetRepository: store.editor.githubSettings.targetRepository,
    }),
    'TitleBar',
  )

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
        flexGrow: 0,
        height: 40,
        fontWeight: 600,
        letterSpacing: 0.2,
        alignItems: 'center',
        justifyContent: 'space-between',
      }}
    >
      <div
        style={{
          display: 'flex',
          height: '100%',
          alignItems: 'center',
          flex: '1 1 0px',
          gap: 10,
        }}
      >
        <AppLogo onClick={toggleLeftPanel} />
        {when(
          loggedIn,
          <>
            <MenuTile
              selected={isCodeEditorVisible}
              icon={<LargerIcons.Code />}
              onClick={toggleCodeEditorVisible}
              size='large'
            />
            <MenuTile
              selected={isPreviewPaneVisible}
              icon={<LargerIcons.PreviewPane />}
              onClick={togglePreviewPaneVisible}
              size='large'
            />
            {when(
              targetRepository == null,
              <SquareButton color={colorTheme.fg2.value} onClick={toggleLeftPanel}>
                <>Connect To GitHub</>
              </SquareButton>,
            )}
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
      </div>

      <div>
        <ProjectTitle>{projectName}</ProjectTitle>
      </div>
      <div style={{ display: 'flex', height: '100%' }}>
        <div style={{ display: 'flex', alignItems: 'center', paddingRight: 24 }}>
          <TestMenu />
        </div>
        {unless(
          loggedIn,
          <SquareButton color={colorTheme.primary.value} onClick={onClickLoginNewTab}>
            Sign In To Save
          </SquareButton>,
        )}
      </div>
    </SimpleFlexRow>
  )
})

export default TitleBar
