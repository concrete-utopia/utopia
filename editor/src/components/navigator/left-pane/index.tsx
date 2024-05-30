/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { FlexColumn, FlexRow, UtopiaStyles, UtopiaTheme, colorTheme } from '../../../uuiui'
import { MenuTab } from '../../../uuiui/menu-tab'
import type { EditorAction, EditorDispatch, LoginState } from '../../editor/action-types'
import { setLeftMenuTab } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import type { DerivedState, EditorState } from '../../editor/store/editor-state'
import { LeftMenuTab } from '../../editor/store/editor-state'
import { LowPriorityStoreProvider } from '../../editor/store/store-context-providers'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { TitleBarProjectTitle } from '../../titlebar/title-bar'
import { NavigatorComponent } from '../navigator'
import { ContentsPane } from './contents-pane'
import { GithubPane } from './github-pane'
import type { StoredPanel } from '../../canvas/stored-layout'
import { useIsMyProject } from '../../editor/store/collaborative-editing'
import { when } from '../../../utils/react-conditionals'
import { PagesPane } from './pages-pane'
import { getRemixRootFile } from '../../canvas/remix/remix-utils'
import { getRemixRootDir } from '../../editor/store/remix-derived-data'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const LeftPaneComponentId = 'left-pane'

export const LeftPaneContentId = 'left-pane-content'

interface LeftPaneComponentProps {
  panelData: StoredPanel
}

export const LeftPaneComponent = React.memo<LeftPaneComponentProps>((props) => {
  const selectedTab = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.leftMenu.selectedTab,
    'LeftPaneComponent selectedTab',
  )

  const dispatch = useDispatch()

  const onClickTab = React.useCallback(
    (menuTab: LeftMenuTab) => {
      let actions: Array<EditorAction> = []
      actions.push(setLeftMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch],
  )

  const onClickPagesTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Pages)
  }, [onClickTab])

  const onClickProjectTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Project)
  }, [onClickTab])

  const onClickNavigatorTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Navigator)
  }, [onClickTab])

  const onClickGithubTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Github)
  }, [onClickTab])

  const isMyProject = useIsMyProject()

  const isRemixProject = useEditorState(
    Substores.projectContents,
    (store) =>
      getRemixRootFile(
        getRemixRootDir(store.editor.projectContents),
        store.editor.projectContents,
      ) != null,
    'LeftPaneComponent isRemixProject',
  )
  return (
    <LowPriorityStoreProvider>
      <FlexColumn
        style={{
          overscrollBehavior: 'contain',
          backgroundColor: colorTheme.leftPaneBackground.value,
          borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
          boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
          overflow: 'hidden',
          flex: 1,
        }}
      >
        <TitleBarProjectTitle panelData={props.panelData} />
        <div
          id={LeftPaneComponentId}
          className='leftPane'
          style={{
            flexGrow: 1,
            position: 'relative',
            color: colorTheme.fg1.value,
            display: 'flex',
            flexDirection: 'column',
            overflowY: 'hidden',
          }}
        >
          {when(
            isMyProject,
            <FlexRow style={{ marginBottom: 10, gap: 10 }} css={undefined}>
              {when(
                isRemixProject,
                <MenuTab
                  label={'Pages'}
                  selected={selectedTab === LeftMenuTab.Pages}
                  onClick={onClickPagesTab}
                />,
              )}
              <MenuTab
                label={'Navigator'}
                selected={selectedTab === LeftMenuTab.Navigator}
                onClick={onClickNavigatorTab}
              />
              <MenuTab
                label={'Project'}
                selected={selectedTab === LeftMenuTab.Project}
                onClick={onClickProjectTab}
              />
              <MenuTab
                label={'Github'}
                selected={selectedTab === LeftMenuTab.Github}
                onClick={onClickGithubTab}
              />
            </FlexRow>,
          )}
          {when(isRemixProject && selectedTab === LeftMenuTab.Pages, <PagesPane />)}
          {when(selectedTab === LeftMenuTab.Navigator, <NavigatorComponent />)}
          {when(isMyProject && selectedTab === LeftMenuTab.Project, <ContentsPane />)}
          {when(isMyProject && selectedTab === LeftMenuTab.Github, <GithubPane />)}
        </div>
      </FlexColumn>
    </LowPriorityStoreProvider>
  )
})
