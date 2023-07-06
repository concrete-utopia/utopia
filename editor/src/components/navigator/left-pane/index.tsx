/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { colorTheme } from '../../../uuiui'
import { User } from '../../../uuiui-deps'
import { MenuTab } from '../../../uuiui/menu-tab'
import { useIsMyProject } from '../../common/server-hooks'
import type { EditorAction, EditorDispatch, LoginState } from '../../editor/action-types'
import { clearSelection, setLeftMenuTab } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import type { DerivedState, EditorState } from '../../editor/store/editor-state'
import { LeftMenuTab, LeftPaneDefaultWidth } from '../../editor/store/editor-state'
import { LowPriorityStoreProvider } from '../../editor/store/store-context-providers'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { ContentsPane } from './contents-pane'
import { ForksGiven } from './forks-given'
import { GithubPane } from './github-pane'
import { LoggedOutPane } from './logged-out-pane'
import { SettingsPane } from './settings-pane'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const LeftPaneComponentId = 'left-pane'

export const LeftPaneOverflowScrollId = 'left-pane-overflow-scroll'

export const LeftPaneComponent = React.memo(() => {
  const selectedTab = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.leftMenu.selectedTab,
    'LeftPaneComponent selectedTab',
  )

  const dispatch = useDispatch()

  const loggedIn = useEditorState(
    Substores.restOfStore,
    (store) => User.isLoggedIn(store.userState.loginState),
    'LeftPaneComponent loggedIn',
  )

  const onClickTab = React.useCallback(
    (menuTab: LeftMenuTab) => {
      let actions: Array<EditorAction> = []
      actions.push(setLeftMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch],
  )

  const onClickProjectTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Project)
  }, [onClickTab])

  const onClickSettingsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Settings)
  }, [onClickTab])

  const onClickGithubTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Github)
  }, [onClickTab])

  return (
    <LowPriorityStoreProvider>
      <div
        id={LeftPaneComponentId}
        className='leftPane'
        style={{
          height: '100%',
          position: 'relative',
          backgroundColor: colorTheme.leftPaneBackground.value,
          color: colorTheme.fg1.value,
          paddingLeft: 4,
          width: LeftPaneDefaultWidth,
        }}
      >
        <div
          id={LeftPaneOverflowScrollId}
          className='overflow-y-scroll'
          style={{
            height: '100%',
            flexGrow: 1,
          }}
          onMouseDown={(mouseEvent: React.MouseEvent<HTMLDivElement>) => {
            if (mouseEvent.target instanceof HTMLDivElement) {
              if (mouseEvent.target.id === LeftPaneOverflowScrollId) {
                dispatch([clearSelection()])
              }
            }
          }}
        >
          <UIGridRow
            variant='<--1fr--><--1fr--><--1fr-->'
            padded={false}
            css={{ gridColumnGap: 0 }}
            style={{ alignItems: 'stretch', marginBottom: 10 }}
          >
            <MenuTab
              label={'Project'}
              selected={selectedTab === LeftMenuTab.Project}
              onClick={onClickProjectTab}
            />
            <MenuTab
              label={'Settings'}
              selected={selectedTab === LeftMenuTab.Settings}
              onClick={onClickSettingsTab}
            />
            <MenuTab
              label={'Github'}
              selected={selectedTab === LeftMenuTab.Github}
              onClick={onClickGithubTab}
            />
          </UIGridRow>

          {selectedTab === LeftMenuTab.Project ? <ContentsPane /> : null}
          {selectedTab === LeftMenuTab.Settings ? <SettingsPane /> : null}
          {selectedTab === LeftMenuTab.Github ? <GithubPane /> : null}
          {loggedIn ? null : <LoggedOutPane />}
        </div>
      </div>
    </LowPriorityStoreProvider>
  )
})
