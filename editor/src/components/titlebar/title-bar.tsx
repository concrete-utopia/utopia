import React, { useCallback } from 'react'
import { auth0Url } from '../../common/env-vars'
import { LargerIcons, MenuIcons, SimpleFlexRow } from '../../uuiui'
import { EditorAction } from '../editor/action-types'
import { setPanelVisibility, togglePanel } from '../editor/actions/action-creators'
import { useEditorState } from '../editor/store/store-hook'
import { MenuTile } from './menu-tile'
import { FullHeightButton, RoundedButton, TextButton } from './buttons'
import { TestMenu } from './test-menu'

const AppLogo: React.FC<{ onClick: () => void }> = ({ onClick }) => (
  <div
    onClick={onClick}
    style={{
      margin: '0 20px 0 0',
      cursor: 'pointer',
    }}
  >
    <MenuIcons.Smiangle />
  </div>
)

interface ProjectTitleProps {}

const ProjectTitle: React.FC<React.PropsWithChildren<ProjectTitleProps>> = ({ children }) => {
  return <div style={{ fontWeight: 400, fontSize: 12, padding: '0 10px' }}>{children}</div>
}

const TitleBar = React.memo(() => {
  const { dispatch, loginState, projectName, isCodeEditorVisible, isPreviewPaneVisible } =
    useEditorState(
      (store) => ({
        dispatch: store.dispatch,
        loginState: store.userState.loginState,
        projectName: store.editor.projectName,
        isCodeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
        isPreviewPaneVisible: store.editor.preview.visible,
      }),
      'TitleBar',
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

  const loggedIn = loginState.type === 'LOGGED_IN'

  return (
    <SimpleFlexRow
      style={{
        padding: '0 10px 0 10px',
        flexGrow: 0,
        height: 40,
        fontWeight: 600,
        letterSpacing: 0.2,
        alignItems: 'center',
        justifyContent: 'space-between',
      }}
    >
      <div style={{ display: 'flex', height: '100%', alignItems: 'center' }}>
        <AppLogo onClick={toggleLeftPanel} />
        {loggedIn ? (
          <>
            <span>
              <MenuTile
                selected={isCodeEditorVisible}
                icon={<LargerIcons.Code />}
                onClick={toggleCodeEditorVisible}
                size='large'
              />
            </span>
            <span>
              <MenuTile
                selected={isPreviewPaneVisible}
                icon={<LargerIcons.PreviewPane />}
                onClick={togglePreviewPaneVisible}
                size='large'
              />
            </span>
          </>
        ) : null}
        {/* <FullHeightButton onClick={exportToGithub}>Export to Github</FullHeightButton> */}
      </div>

      <div>
        <ProjectTitle>{projectName}</ProjectTitle>
      </div>

      <div style={{ display: 'flex', height: '100%' }}>
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <TestMenu />
        </div>
        {/* <TextButton onClick={exportToGithub}>Fork</TextButton> */}
        {loggedIn ? null : (
          <FullHeightButton onClick={onClickLoginNewTab}>Sign in to Save</FullHeightButton>
        )}
      </div>
    </SimpleFlexRow>
  )
})

export default TitleBar
