/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { ResizableFlexColumn, UtopiaTheme, colorTheme } from '../../../uuiui'
import type { ResizableProps } from '../../../uuiui-deps'
import { User } from '../../../uuiui-deps'
import { MenuTab } from '../../../uuiui/menu-tab'
import { useIsMyProject } from '../../common/server-hooks'
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
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { ContentsPane } from './contents-pane'
import { ForksGiven } from './forks-given'
import { GithubPane } from './github-pane'
import { LoggedOutPane } from './logged-out-pane'
import { SettingsPane } from './settings-pane'
import { NavigatorComponent } from '../navigator'
import { usePubSubAtom } from '../../../core/shared/atom-with-pub-sub'
import type { ResizeCallback } from 're-resizable'
import { PanelTitleBar } from '../../canvas/floating-panels'
import type { Direction } from 're-resizable/lib/resizer'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const LeftPaneComponentId = 'left-pane'

export const LeftPaneOverflowScrollId = 'left-pane-overflow-scroll'

interface LeftPaneComponentProps {
  width: number
  height: number
  onResizeStop: (menuName: 'navigator', direction: Direction, width: number, height: number) => void
  resizableConfig: ResizableProps
}

export const LeftPaneComponent = React.memo<LeftPaneComponentProps>((props) => {
  const { onResizeStop, width, height } = props
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

  const onClickSettingsTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Settings)
  }, [onClickTab])

  const onClickGithubTab = React.useCallback(() => {
    onClickTab(LeftMenuTab.Github)
  }, [onClickTab])

  const onLeftPanelResizeStop = React.useCallback<ResizeCallback>(
    (_event, _direction, _ref, delta) => {
      onResizeStop('navigator', _direction, _ref?.clientWidth, _ref?.clientHeight)
    },
    [onResizeStop],
  )
  const onLeftPanelResize = React.useCallback<ResizeCallback>(
    (_event, _direction, _ref, delta) => {
      const newWidth = _ref?.clientWidth
      const newHeight = _ref?.clientHeight
      if (newWidth != null && newHeight != null) {
        onResizeStop('navigator', _direction, newWidth, newHeight)
      }
    },
    [onResizeStop],
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
      <ResizableFlexColumn
        onResizeStop={onLeftPanelResizeStop}
        onResize={onLeftPanelResize}
        defaultSize={{
          width: width,
          height: '100%',
        }}
        size={{
          width: width,
          height: height,
        }}
        style={{
          overscrollBehavior: 'contain',
          backgroundColor: colorTheme.leftPaneBackground.value,
          borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
          boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
          display: 'flex',
          flexDirection: 'column',
          overflow: 'hidden',
        }}
        {...props.resizableConfig}
      >
        <PanelTitleBar />
        <div
          id={LeftPaneComponentId}
          className='leftPane'
          style={{
            height: '100%',
            position: 'relative',
            color: colorTheme.fg1.value,
            display: 'flex',
            overflow: 'scroll',
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
              variant='|--67px--||--67px--||--67px--||--67px--|'
              padded={false}
              css={{ gridColumnGap: 0 }}
              style={{ alignItems: 'stretch', marginBottom: 10 }}
            >
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

            {selectedTab === LeftMenuTab.Navigator ? <NavigatorComponent /> : null}
            {selectedTab === LeftMenuTab.Project ? <ContentsPane /> : null}
            {selectedTab === LeftMenuTab.Settings ? <SettingsPane /> : null}
            {selectedTab === LeftMenuTab.Github ? <GithubPane /> : null}
            {loggedIn ? null : <LoggedOutPane />}
          </div>
        </div>
      </ResizableFlexColumn>
    </LowPriorityStoreProvider>
  )
})
