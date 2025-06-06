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
import { foldEither } from '../../../core/shared/either'

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

  const onMouseDownTab = React.useCallback(
    (menuTab: LeftMenuTab) => {
      let actions: Array<EditorAction> = []
      actions.push(setLeftMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch],
  )

  const onMouseDownPagesTab = React.useCallback(() => {
    onMouseDownTab(LeftMenuTab.Pages)
  }, [onMouseDownTab])

  const onMouseDownProjectTab = React.useCallback(() => {
    onMouseDownTab(LeftMenuTab.Project)
  }, [onMouseDownTab])

  const onMouseDownNavigatorTab = React.useCallback(() => {
    onMouseDownTab(LeftMenuTab.Navigator)
  }, [onMouseDownTab])

  const onMouseDownGithubTab = React.useCallback(() => {
    onMouseDownTab(LeftMenuTab.Github)
  }, [onMouseDownTab])

  const isMyProject = useIsMyProject()

  const curriedRequireFn = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.codeResultCache.curriedRequireFn,
    'LeftPaneComponent curriedRequireFn',
  )

  const isRemixProject = useEditorState(
    Substores.projectContents,
    (store) => {
      const rootDir = getRemixRootDir(store.editor.projectContents, curriedRequireFn)
      return foldEither(
        () => false,
        (dir) => getRemixRootFile(dir, store.editor.projectContents) != null,
        rootDir,
      )
    },
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
                  onMouseDown={onMouseDownPagesTab}
                />,
              )}
              <MenuTab
                label={'Navigator'}
                selected={selectedTab === LeftMenuTab.Navigator}
                onMouseDown={onMouseDownNavigatorTab}
              />
              <MenuTab
                label={'Project'}
                selected={selectedTab === LeftMenuTab.Project}
                onMouseDown={onMouseDownProjectTab}
              />
              <MenuTab
                label={'Github'}
                selected={selectedTab === LeftMenuTab.Github}
                onMouseDown={onMouseDownGithubTab}
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
