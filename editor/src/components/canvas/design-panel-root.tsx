/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import * as EditorActions from '../editor/actions/action-creators'
import { RightMenuTab } from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { InspectorEntryPoint } from '../inspector/inspector'
import { CanvasWrapperComponent } from './canvas-wrapper-component'
import { CodeEditorWrapper } from '../code-editor/code-editor-container'
import {
  SimpleFlexRow,
  UtopiaTheme,
  SimpleFlexColumn,
  FlexColumn,
  colorTheme,
  UtopiaStyles,
} from '../../uuiui'
import { ErrorsPane } from '../code-editor/errors-pane'
import { CanvasStrategyInspector } from './canvas-strategies/canvas-strategy-inspector'
import { IS_TEST_ENVIRONMENT, getQueryParam } from '../../common/env-vars'
import { when } from '../../utils/react-conditionals'
import { useDispatch } from '../editor/store/dispatch-context'
import { GridPanelsContainer } from './grid-panels-container'
import { TitleBarCode, TitleBarUserProfile } from '../titlebar/title-bar'
import type { EditorAction } from '../editor/action-types'
import { SettingsPane } from '../navigator/left-pane/settings-pane'
import { MenuTab } from '../../uuiui/menu-tab'
import { FlexRow } from 'utopia-api'
import type { StoredPanel } from './stored-layout'
import { useRoom, useStatus } from '../../../liveblocks.config'
import { CommentsPane } from '../inspector/comments-pane'
import { EditorModes, isCommentMode } from '../editor/editor-modes'
import { useAllowedToEditProject } from '../editor/store/collaborative-editing'
import { useCanComment } from '../../core/commenting/comment-hooks'
import { ElementsOutsideVisibleAreaIndicator } from '../editor/elements-outside-visible-area-indicator'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { RollYourOwnFeaturesPane } from '../navigator/left-pane/roll-your-own-pane'
import type { AnimationScope } from 'framer-motion'

function isCodeEditorEnabled(): boolean {
  if (typeof window !== 'undefined') {
    return getQueryParam('code_editor_disabled') !== 'true'
  } else {
    return true
  }
}

const DesignPanelRootInner = React.memo(() => {
  const roomStatus = useStatus()

  const room = useRoom()
  const loginStateType = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState.type,
    'DesignPanelRootInner loginStateType',
  )

  React.useEffect(() => {
    const roomConsideredDisconnected = roomStatus === 'disconnected' || roomStatus === 'initial'
    if (loginStateType === 'LOGGED_IN' && roomConsideredDisconnected) {
      room.reconnect()
    }
  }, [loginStateType, room, roomStatus])

  return (
    <>
      <SimpleFlexRow
        className='CanvasCodeRow'
        style={{
          position: 'relative',
          flexDirection: 'row',
          alignItems: 'stretch',
          overflowX: 'hidden',
          flexGrow: 1,
          flexShrink: 0,
        }}
      >
        {
          <SimpleFlexColumn
            style={{
              flexGrow: 1,
              overflow: 'hidden',
              position: 'relative',
            }}
          >
            <CanvasWrapperComponent />
            <ElementsOutsideVisibleAreaIndicator />
            <GridPanelsContainer />
          </SimpleFlexColumn>
        }
      </SimpleFlexRow>
    </>
  )
})

export const DesignPanelRoot = React.memo((props: { animationScope: AnimationScope<any> }) => {
  return (
    <>
      <SimpleFlexRow
        ref={props.animationScope}
        className='OpenFileEditorShell'
        style={{
          position: 'relative',
          flexGrow: 1,
          alignItems: 'stretch',
          overflowX: 'hidden',
        }}
      >
        <DesignPanelRootInner />
      </SimpleFlexRow>
    </>
  )
})
DesignPanelRoot.displayName = 'DesignPanelRoot'

interface ResizableRightPaneProps {
  panelData: StoredPanel
}

export const RightPane = React.memo<ResizableRightPaneProps>((props) => {
  const selectedTab = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.selectedTab,
    'ResizableRightPane selectedTab',
  )

  const editorModeRef = useRefEditorState((store) => store.editor.mode)

  const isRightMenuExpanded = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.visible,
    'DesignPanelRoot isRightMenuExpanded',
  )
  const dispatch = useDispatch()

  const onClickTab = React.useCallback(
    (menuTab: RightMenuTab) => {
      const actions: Array<EditorAction> = [EditorActions.setRightMenuTab(menuTab)]
      if (isCommentMode(editorModeRef.current) && menuTab !== RightMenuTab.Comments) {
        actions.push(EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')))
      }
      dispatch(actions)
    },
    [dispatch, editorModeRef],
  )

  const onClickInsertTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Insert)
  }, [onClickTab])

  const onClickCommentsTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Comments)
  }, [onClickTab])

  const onClickInspectorTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Inspector)
  }, [onClickTab])

  const onClickSettingsTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Settings)
  }, [onClickTab])

  const onClickRollYourOwnTab = React.useCallback(() => {
    onClickTab(RightMenuTab.RollYourOwn)
  }, [onClickTab])

  const canComment = useCanComment()

  const allowedToEdit = useAllowedToEditProject()

  if (!isRightMenuExpanded) {
    return null
  }

  return (
    <FlexColumn
      style={{
        flex: 1,
        overflow: 'hidden',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
      }}
    >
      <TitleBarUserProfile panelData={props.panelData} />
      <FlexRow
        style={{
          marginBottom: 10,
          gap: 2,
          alignSelf: 'stretch',
          flexShrink: 0,
          overflowX: 'scroll',
        }}
        css={undefined}
      >
        <MenuTab
          label={'Inspector'}
          selected={selectedTab === RightMenuTab.Inspector}
          onClick={onClickInspectorTab}
        />
        {when(
          allowedToEdit,
          <>
            {when(
              IS_TEST_ENVIRONMENT,
              <MenuTab
                label={'Insert'}
                selected={selectedTab === RightMenuTab.Insert}
                onClick={onClickInsertTab}
              />,
            )}
          </>,
        )}
        {when(
          canComment,
          <MenuTab
            testId='comments-tab'
            label={'Comments'}
            selected={selectedTab === RightMenuTab.Comments}
            onClick={onClickCommentsTab}
          />,
        )}
        <MenuTab
          label={'Settings'}
          selected={selectedTab === RightMenuTab.Settings}
          onClick={onClickSettingsTab}
        />
        {when(
          isFeatureEnabled('Roll Your Own'),
          <MenuTab
            label={'RYO'}
            selected={selectedTab === RightMenuTab.RollYourOwn}
            onClick={onClickRollYourOwnTab}
          />,
        )}
      </FlexRow>
      <SimpleFlexRow
        className='Inspector-entrypoint'
        id='inspector-root'
        style={{
          display: 'flex',
          flexDirection: 'column',
          flexGrow: 1,
          position: 'relative',
          color: colorTheme.fg1.value,
          overflowY: 'scroll',
          backgroundColor: colorTheme.inspectorBackground.value,
        }}
      >
        {when(selectedTab === RightMenuTab.Inspector, <InspectorEntryPoint />)}
        {when(selectedTab === RightMenuTab.Settings, <SettingsPane />)}
        {when(selectedTab === RightMenuTab.Comments, <CommentsPane />)}
        {when(selectedTab === RightMenuTab.RollYourOwn, <RollYourOwnFeaturesPane />)}
      </SimpleFlexRow>
      <CanvasStrategyInspector />
    </FlexColumn>
  )
})

interface CodeEditorPaneProps {
  panelData: StoredPanel
  small: boolean
}

export const CodeEditorPane = React.memo<CodeEditorPaneProps>((props) => {
  const codeEditorEnabled = isCodeEditorEnabled()

  return (
    <FlexColumn
      className='resizableFlexColumnCanvasCode'
      style={{
        flex: 1,
        contain: 'layout',
        overflow: 'hidden',
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
        background: colorTheme.bg1.value,
      }}
    >
      <TitleBarCode panelData={props.panelData} />
      <div
        style={{
          flex: 1,
          contain: 'layout',
        }}
      >
        {/* this FlexColumn needs to be alone in the parent div, otherwise the 100% height will mess things up */}
        <FlexColumn
          style={{
            transformOrigin: 'top left',
            width: props.small ? 'calc(100%/0.7)' : '100%',
            height: props.small ? 'calc(100%/0.7)' : '100%',
            transform: props.small ? 'scale(0.7)' : undefined,
            flex: 1,
          }}
        >
          {when(codeEditorEnabled, <CodeEditorWrapper />)}
        </FlexColumn>
      </div>
      <FlexColumn
        style={{
          contain: 'layout',
          zoom: props.small ? 0.7 : undefined,
        }}
      >
        <ErrorsPane />
      </FlexColumn>
    </FlexColumn>
  )
})
