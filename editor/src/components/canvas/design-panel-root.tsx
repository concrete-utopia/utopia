/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import type { ResizeDirection } from 're-resizable'
import { Resizable } from 're-resizable'
import React from 'react'
import * as EditorActions from '../editor/actions/action-creators'
import { RightMenuTab } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { InspectorEntryPoint } from '../inspector/inspector'
import { CanvasWrapperComponent } from './canvas-wrapper-component'
import { CodeEditorWrapper } from '../code-editor/code-editor-container'
import {
  SimpleFlexRow,
  UtopiaTheme,
  SimpleFlexColumn,
  useColorTheme,
  LargerIcons,
  FlexColumn,
  colorTheme,
} from '../../uuiui'
import { ConsoleAndErrorsPane } from '../code-editor/console-and-errors-pane'
import { InspectorWidthAtom } from '../inspector/common/inspector-atoms'
import { useAtom } from 'jotai'
import { CanvasStrategyInspector } from './canvas-strategies/canvas-strategy-inspector'
import { getQueryParam } from '../../common/env-vars'
import { unless, when } from '../../utils/react-conditionals'
import { InsertMenuPane } from '../navigator/insert-menu-pane'
import { useDispatch } from '../editor/store/dispatch-context'
import { LeftPaneComponent } from '../navigator/left-pane'
import { GridPanelsContainer } from './grid-panels-container'
import type { ResizableProps } from '../../uuiui-deps'
import type { Direction } from 're-resizable/lib/resizer'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { TitleBarCode, TitleBarUserProfile } from '../titlebar/title-bar'
import type { EditorAction } from '../editor/action-types'
import { SettingsPane } from '../navigator/left-pane/settings-pane'
import { MenuTab } from '../../uuiui/menu-tab'
import { FlexRow } from 'utopia-api'
import type { StoredPanel } from './stored-layout'
import { CommentSection } from '../inspector/sections/comment-section'

interface NumberSize {
  width: number
  height: number
}

function isCodeEditorEnabled(): boolean {
  if (typeof window !== 'undefined') {
    return getQueryParam('code_editor_disabled') !== 'true'
  } else {
    return true
  }
}

const TopMenuHeight = 35
// height so that the bottom border on the top menu aligns
// with the top border of the first inspector section

const NothingOpenCard = React.memo(() => {
  const dispatch = useDispatch()
  const handleOpenCanvasClick = React.useCallback(() => {
    dispatch([EditorActions.setPanelVisibility('canvas', true)])
  }, [dispatch])
  const handleOpenCodeEditorClick = React.useCallback(() => {
    dispatch([EditorActions.setPanelVisibility('codeEditor', true)])
  }, [dispatch])
  const handleOpenBothCodeEditorAndDesignToolClick = React.useCallback(() => {
    dispatch([
      EditorActions.setPanelVisibility('codeEditor', true),
      EditorActions.setPanelVisibility('canvas', true),
    ])
  }, [dispatch])

  return (
    <div
      role='card'
      style={{
        width: 180,
        height: 240,
        padding: 12,
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'space-around',
        borderRadius: 4,
        backgroundColor: 'white',
        border: '1px solid lightgrey',
      }}
    >
      <LargerIcons.PixelatedPalm
        color='primary'
        style={{ width: 42, height: 42, imageRendering: 'pixelated' }}
      />
      <div style={{ textAlign: 'center' }}>
        <h3 style={{ fontWeight: 600, fontSize: 11 }}>Get Started</h3>
        <p style={{ lineHeight: '1.7em', whiteSpace: 'normal' }}>
          Start building with the &nbsp;
          <span
            role='button'
            style={{ cursor: 'pointer', color: colorTheme.primary.value }}
            onClick={handleOpenCanvasClick}
          >
            canvas
          </span>
          ,&nbsp;
          <span
            role='button'
            style={{ cursor: 'pointer', color: colorTheme.primary.value }}
            onClick={handleOpenCodeEditorClick}
          >
            code editor
          </span>
          ,&nbsp; or{' '}
          <span
            role='button'
            style={{ cursor: 'pointer', color: colorTheme.primary.value }}
            onClick={handleOpenBothCodeEditorAndDesignToolClick}
          >
            both
          </span>
          .
        </p>
      </div>
    </div>
  )
})

const DesignPanelRootInner = React.memo(() => {
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
            <GridPanelsContainer />
          </SimpleFlexColumn>
        }
      </SimpleFlexRow>
    </>
  )
})

export const DesignPanelRoot = React.memo(() => {
  return (
    <>
      <SimpleFlexRow
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

  const isRightMenuExpanded = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.visible,
    'DesignPanelRoot isRightMenuExpanded',
  )
  const dispatch = useDispatch()

  const onClickTab = React.useCallback(
    (menuTab: RightMenuTab) => {
      let actions: Array<EditorAction> = []
      actions.push(EditorActions.setRightMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch],
  )

  const onClickInsertTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Insert)
  }, [onClickTab])

  const onClickInspectorTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Inspector)
  }, [onClickTab])

  const onClickSettingsTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Settings)
  }, [onClickTab])

  const onClickCommentsTab = React.useCallback(() => {
    onClickTab(RightMenuTab.Comments)
  }, [onClickTab])

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
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
      }}
    >
      <TitleBarUserProfile panelData={props.panelData} />
      <FlexRow
        style={{ marginBottom: 10, gap: 10, alignSelf: 'stretch', flexShrink: 0 }}
        css={undefined}
      >
        <MenuTab
          label={'Inspector'}
          selected={selectedTab === RightMenuTab.Inspector}
          onClick={onClickInspectorTab}
        />
        <MenuTab
          label={'Comments'}
          selected={selectedTab === RightMenuTab.Comments}
          onClick={onClickCommentsTab}
        />
        <MenuTab
          label={'Insert'}
          selected={selectedTab === RightMenuTab.Insert}
          onClick={onClickInsertTab}
        />
        <MenuTab
          label={'Settings'}
          selected={selectedTab === RightMenuTab.Settings}
          onClick={onClickSettingsTab}
        />
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
        {when(selectedTab === RightMenuTab.Insert, <InsertMenuPane />)}
        {when(selectedTab === RightMenuTab.Inspector, <InspectorEntryPoint />)}
        {when(selectedTab === RightMenuTab.Settings, <SettingsPane />)}
        {when(selectedTab === RightMenuTab.Comments, <CommentSection />)}
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
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
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
        <ConsoleAndErrorsPane />
      </FlexColumn>
    </FlexColumn>
  )
})
