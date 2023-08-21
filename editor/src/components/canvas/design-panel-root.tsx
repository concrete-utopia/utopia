import type { Enable, ResizeCallback, ResizeDirection } from 're-resizable'
import { Resizable } from 're-resizable'
import React from 'react'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import * as EditorActions from '../editor/actions/action-creators'

import { ConsoleLog, RightMenuTab } from '../editor/store/editor-state'

import { Substores, useEditorState } from '../editor/store/store-hook'
import { InspectorEntryPoint } from '../inspector/inspector'
import { CanvasWrapperComponent } from './canvas-wrapper-component'

import { CodeEditorWrapper } from '../code-editor/code-editor-container'
import { NavigatorComponent } from '../navigator/navigator'
import {
  SimpleFlexRow,
  UtopiaTheme,
  UtopiaStyles,
  SimpleFlexColumn,
  background,
  useColorTheme,
  Icons,
  LargerIcons,
} from '../../uuiui'

import { ConsoleAndErrorsPane } from '../code-editor/console-and-errors-pane'
import { FloatingInsertMenu } from './ui/floating-insert-menu'
import { canvasPoint } from '../../core/shared/math-utils'
import { InspectorWidthAtom } from '../inspector/common/inspector-atoms'
import { useAtom } from 'jotai'
import { CanvasStrategyInspector } from './canvas-strategies/canvas-strategy-inspector'
import { getQueryParam } from '../../common/env-vars'
import { when } from '../../utils/react-conditionals'
import { InsertMenuPane } from '../navigator/insert-menu-pane'
import { CanvasToolbar } from '../editor/canvas-toolbar'
import { useDispatch } from '../editor/store/dispatch-context'
import { LeftPaneComponent } from '../navigator/left-pane'
import { FloatingPanel, FloatingPanelsContainer } from './floating-panels'

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
            {/* <CodeEditorPane /> */}
            {/* <LeftPaneComponent /> */}
            <CanvasWrapperComponent />
            <FloatingInsertMenu />
            {/* <ResizableRightPane /> */}
            <FloatingPanelsContainer />
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
  enabledDirection: Enable
}

export const ResizableRightPane = React.memo<ResizableRightPaneProps>((props) => {
  const colorTheme = useColorTheme()
  const [, updateInspectorWidth] = useAtom(InspectorWidthAtom)

  const resizableRef = React.useRef<Resizable>(null)
  const [width, setWidth] = React.useState<number>(UtopiaTheme.layout.inspectorSmallWidth)

  const onResize = React.useCallback(() => {
    const newWidth = resizableRef.current?.size.width
    if (newWidth != null) {
      // we have to use the instance ref to directly access the get size() getter, because re-resize's API only wants to tell us deltas, but we need the snapped width
      setWidth(newWidth)
      updateInspectorWidth(newWidth > UtopiaTheme.layout.inspectorSmallWidth ? 'wide' : 'regular')
    }
  }, [updateInspectorWidth])

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
  if (!isRightMenuExpanded) {
    return null
  }

  return (
    <Resizable
      ref={resizableRef}
      defaultSize={{
        width: UtopiaTheme.layout.inspectorSmallWidth,
        height: '100%',
      }}
      size={{
        width: width,
        height: '100%',
      }}
      style={{
        transition: 'width 100ms ease-in-out',
        overflow: 'hidden',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
      }}
      snap={{
        x: [UtopiaTheme.layout.inspectorSmallWidth, UtopiaTheme.layout.inspectorLargeWidth],
      }}
      onResizeStart={onResize}
      onResize={onResize}
      onResizeStop={onResize}
      enable={props.enabledDirection}
    >
      <div className='handle' style={{ height: 34, width: '100%' }}>
        draggable title
      </div>
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
        {selectedTab === RightMenuTab.Insert && <InsertMenuPane />}
        {selectedTab === RightMenuTab.Inspector && <InspectorEntryPoint />}
      </SimpleFlexRow>
      <CanvasStrategyInspector />
    </Resizable>
  )
})

interface CodeEditorPaneProps {
  small: boolean
}

export const CodeEditorPane = React.memo<CodeEditorPaneProps>((props) => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()
  const interfaceDesigner = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.interfaceDesigner,
    'CodeEditorPane interfaceDesigner',
  )

  const codeEditorEnabled = isCodeEditorEnabled()
  const onResizeStop = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: NumberSize,
    ) => {
      dispatch([EditorActions.resizeInterfaceDesignerCodePane(delta.width)])
    },
    [dispatch],
  )

  return (
    <Resizable
      defaultSize={{
        width: interfaceDesigner.codePaneWidth,
        height: '100%',
      }}
      size={{
        width: interfaceDesigner.codePaneWidth,
        height: '100%',
      }}
      onResizeStop={onResizeStop}
      enable={{
        top: false,
        right: true,
        bottom: false,
        topRight: false,
        bottomRight: false,
        bottomLeft: false,
        topLeft: false,
      }}
      className='resizableFlexColumnCanvasCode'
      style={{
        display: interfaceDesigner.codePaneVisible ? 'block' : 'none',
        width: interfaceDesigner.codePaneWidth,
        height: '100%',
        position: 'relative',
        overflow: 'hidden',
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
      }}
    >
      <div className='handle' style={{ height: 34, width: '100%' }}>
        draggable title
      </div>
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
        {when(codeEditorEnabled, <CodeEditorWrapper />)}
        <ConsoleAndErrorsPane />
      </div>
    </Resizable>
  )
})
