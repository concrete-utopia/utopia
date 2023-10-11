/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { FlexRow, ResizableFlexColumn, UtopiaTheme, colorTheme } from '../../../uuiui'
import type { ResizableProps } from '../../../uuiui-deps'
import { User } from '../../../uuiui-deps'
import { MenuTab } from '../../../uuiui/menu-tab'
import type { EditorAction, EditorDispatch, LoginState } from '../../editor/action-types'
import { clearSelection, setLeftMenuTab } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import type { DerivedState, EditorState } from '../../editor/store/editor-state'
import {
  LeftMenuTab,
  LeftPaneDefaultWidth,
  LeftPanelMinWidth,
  LeftPanelWidthAtom,
} from '../../editor/store/editor-state'
import { LowPriorityStoreProvider } from '../../editor/store/store-context-providers'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { ContentsPane } from './contents-pane'
import { ForksGiven } from './forks-given'
import { GithubPane } from './github-pane'
import { SettingsPane } from './settings-pane'
import { NavigatorComponent } from '../navigator'
import { usePubSubAtom } from '../../../core/shared/atom-with-pub-sub'
import type { ResizeCallback } from 're-resizable'
import type { Menu, Pane } from '../../canvas/grid-panels-state'
import type { Direction } from 're-resizable/lib/resizer'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { when } from '../../../utils/react-conditionals'
import { TitleBarProjectTitle } from '../../titlebar/title-bar'
import type { StoredPanel } from '../../canvas/grid-panels-state'

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

  const onClickNavigatorTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Navigator)
  }, [onClickTab])

  const onClickGithubTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Github)
  }, [onClickTab])

  const [leftPanelWidth, setLeftPanelWidth] = usePubSubAtom(LeftPanelWidthAtom)
  const onLeftPanelResizeStop = React.useCallback<ResizeCallback>(
    (_event, _direction, _ref, delta) => {
      setLeftPanelWidth((currentWidth) => currentWidth + delta.width)
    },
    [setLeftPanelWidth],
  )

  const leftMenuExpanded = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.leftMenu.expanded,
    'LeftPaneComponent leftMenuExpanded',
  )

  if (!leftMenuExpanded) {
    return null
  }

  return (
    <LowPriorityStoreProvider>
      <ResizableFlexColumn // TODO resizable is not needed
        enable={{ right: false }}
        onResizeStop={onLeftPanelResizeStop}
        defaultSize={{
          width: '100%', // TODO 100% is sus
          height: '100%',
        }}
        size={{
          width: '100%', // TODO 100% is sus
          height: '100%',
        }}
        style={{
          overscrollBehavior: 'contain',
          backgroundColor: colorTheme.leftPaneBackground.value,
          borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
          boxShadow: UtopiaTheme.panelStyles.shadows.medium,
          display: 'flex',
          flexDirection: 'column',
          overflow: 'hidden',
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
            overflowY: 'scroll',
          }}
        >
          <FlexRow style={{ marginBottom: 10, gap: 10 }} css={undefined}>
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
          </FlexRow>
          {selectedTab === LeftMenuTab.Navigator ? <NavigatorComponent /> : null}
          {selectedTab === LeftMenuTab.Project ? <ContentsPane /> : null}
          {selectedTab === LeftMenuTab.Github ? <GithubPane /> : null}
        </div>
      </ResizableFlexColumn>
    </LowPriorityStoreProvider>
  )
})
