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
import { GridMenuWidth } from './grid-panels-state'
import { GridPanelsContainer } from './grid-panels-container'
import type { Menu, Pane, StoredPanel } from './grid-panels-state'
import type { ResizableProps } from '../../uuiui-deps'
import type { Direction } from 're-resizable/lib/resizer'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { TitleBarCode, TitleBarUserProfile } from '../titlebar/title-bar'
import type { EditorAction } from '../editor/action-types'
import { SettingsPane } from '../navigator/left-pane/settings-pane'
import { MenuTab } from '../../uuiui/menu-tab'
import { FlexRow } from 'utopia-api'

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
  const colorTheme = useColorTheme()
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

export const ResizableRightPane = React.memo<ResizableRightPaneProps>((props) => {
  const defaultInspectorWidth = GridMenuWidth

  const colorTheme = useColorTheme()
  const [, updateInspectorWidth] = useAtom(InspectorWidthAtom)

  const [width, setWidth] = React.useState<number>(defaultInspectorWidth)

  const resizableRef = React.useRef<Resizable>(null)
  const onResize = React.useCallback(() => {
    const newWidth = resizableRef.current?.size.width
    if (newWidth != null) {
      // we have to use the instance ref to directly access the get size() getter, because re-resize's API only wants to tell us deltas, but we need the snapped width
      setWidth(newWidth)
      updateInspectorWidth(newWidth > defaultInspectorWidth ? 'wide' : 'regular')
    }
  }, [updateInspectorWidth, defaultInspectorWidth])

  const selectedTab = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.selectedTab,
    'ResizableRightPane selectedTab',
  )

  const isRightMenuExpanded = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.expanded,
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

  if (!isRightMenuExpanded) {
    return null
  }

  return (
    <Resizable // TODO DELETE ME!
      ref={resizableRef}
      defaultSize={{
        width: '100%', // TODO 100% is sus
        height: '100%',
      }}
      size={{
        width: '100%', // TODO 100% is sus
        height: '100%',
      }}
      style={{
        transition: 'width 100ms ease-in-out',
        overflow: 'hidden',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
      }}
      onResizeStart={onResize}
      onResize={onResize}
      onResizeStop={onResize}
      snap={{
        x: [defaultInspectorWidth, UtopiaTheme.layout.inspectorLargeWidth],
      }}
      enable={{
        left: false,
      }}
    >
      <TitleBarUserProfile panelData={props.panelData} />,
      <SimpleFlexRow
        className='Inspector-entrypoint'
        id='inspector-root'
        style={{
          alignItems: 'stretch',
          flexDirection: 'column',
          width: '100%',
          height: '100%',
          backgroundColor: colorTheme.inspectorBackground.value,
          flexGrow: 0,
          flexShrink: 0,
        }}
      >
        <FlexRow style={{ marginBottom: 10, gap: 10 }} css={undefined}>
          <MenuTab
            label={'Inspector'}
            selected={selectedTab === RightMenuTab.Inspector}
            onClick={onClickInspectorTab}
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
        {when(selectedTab === RightMenuTab.Insert, <InsertMenuPane />)}
        {when(selectedTab === RightMenuTab.Inspector, <InspectorEntryPoint />)}
        {when(selectedTab === RightMenuTab.Settings, <SettingsPane />)}
      </SimpleFlexRow>
      <CanvasStrategyInspector />
    </Resizable>
  )
})

interface CodeEditorPaneProps {
  panelData: StoredPanel
  small: boolean
}

export const CodeEditorPane = React.memo<CodeEditorPaneProps>((props) => {
  const dispatch = useDispatch()
  const interfaceDesigner = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.interfaceDesigner,
    'CodeEditorPane interfaceDesigner',
  )

  const codeEditorEnabled = isCodeEditorEnabled()

  return (
    <Resizable // TODO delete me!
      defaultSize={{
        width: '100%', // TODO 100% is sus
        height: '100%',
      }}
      size={{
        width: '100%', // TODO 100% is sus
        height: '100%',
      }}
      enable={{
        top: false,
        right: false,
        bottom: false,
        topRight: false,
        bottomRight: false,
        bottomLeft: false,
        topLeft: false,
      }}
      className='resizableFlexColumnCanvasCode'
      style={{
        display: props.small ? 'block' : interfaceDesigner.codePaneVisible ? 'flex' : 'none',
        position: 'relative',
        overflow: 'hidden',
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        flexDirection: 'column',
      }}
    >
      <TitleBarCode panelData={props.panelData} />,
      <div
        style={{
          transformOrigin: 'top left',
          width: props.small ? 'calc(100%/0.7)' : '100%',
          height: props.small ? 'calc(100%/0.7)' : '100%',
          transform: props.small ? 'scale(0.7)' : undefined,
          flexDirection: 'column',
          whiteSpace: 'nowrap',
          display: 'flex',
          alignItems: 'stretch',
          justifyContent: 'stretch',
        }}
      >
        <div
          style={{
            display: 'flex',
            height: '100%',
          }}
        >
          {when(codeEditorEnabled, <CodeEditorWrapper />)}
        </div>
        <ConsoleAndErrorsPane />
      </div>
    </Resizable>
  )
})
