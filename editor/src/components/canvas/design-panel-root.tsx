import type { ResizeCallback, ResizeDirection } from 're-resizable'
import { Resizable } from 're-resizable'
import React from 'react'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import * as EditorActions from '../editor/actions/action-creators'

import { ConsoleLog, LeftPanelMinWidth, RightMenuTab } from '../editor/store/editor-state'

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
import type { Size } from '../../core/shared/math-utils'
import { InspectorWidthAtom } from '../inspector/common/inspector-atoms'
import { useAtom } from 'jotai'
import { CanvasStrategyInspector } from './canvas-strategies/canvas-strategy-inspector'
import { getQueryParam } from '../../common/env-vars'
import { unless, when } from '../../utils/react-conditionals'
import { InsertMenuPane } from '../navigator/insert-menu-pane'
import { CanvasToolbar } from '../editor/canvas-toolbar'
import { useDispatch } from '../editor/store/dispatch-context'
import { LeftPaneComponent } from '../navigator/left-pane'
import { FloatingPanelsContainer, PanelTitleBar } from './floating-panels'
import type { Menu, Pane } from './floating-panels'
import type { ResizableProps } from '../../uuiui-deps'
import type { Direction } from 're-resizable/lib/resizer'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { NO_OP } from '../../core/shared/utils'
import { TitleBarEmpty, TitleBarUserProfile, TitleHeight } from '../titlebar/title-bar'

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
  const draggablePanelsEnabled = isFeatureEnabled('Draggable Floating Panels')
  const codeEditorWidth = useEditorState(
    Substores.restOfEditor,
    (store) =>
      store.editor.interfaceDesigner.codePaneVisible
        ? store.editor.interfaceDesigner.codePaneWidth + 10
        : 0,
    'DesignPanelRootInner interfaceDesigner',
  )

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
            {unless(
              draggablePanelsEnabled,
              <div
                id='vscode-editor'
                style={{ height: 'calc(100% - 20px)', position: 'absolute', margin: 10, zIndex: 1 }}
              >
                <CodeEditorPane
                  small={false}
                  width={0}
                  height={0}
                  onResize={NO_OP}
                  setIsResizing={NO_OP}
                  resizableConfig={{
                    enable: {
                      right: true,
                    },
                  }}
                />
              </div>,
            )}
            {unless(
              draggablePanelsEnabled,
              <div
                id='left-pane'
                style={{
                  height: 'calc(100% - 20px)',
                  position: 'absolute',
                  top: 0,
                  left: codeEditorWidth,
                  zIndex: 1,
                  margin: 10,
                }}
              >
                <LeftPaneComponent
                  width={0}
                  height={0}
                  onResize={NO_OP}
                  setIsResizing={NO_OP}
                  resizableConfig={{
                    minWidth: LeftPanelMinWidth,
                    enable: {
                      right: true,
                    },
                  }}
                />
              </div>,
            )}
            <CanvasWrapperComponent />
            <FloatingInsertMenu />
            {unless(
              draggablePanelsEnabled,
              <div
                id='inspector-root'
                style={{
                  height: 'calc(100% - 20px)',
                  position: 'absolute',
                  right: 0,
                  margin: 10,
                }}
              >
                <ResizableRightPane
                  width={0}
                  height={0}
                  onResize={NO_OP}
                  setIsResizing={NO_OP}
                  resizableConfig={{
                    snap: {
                      x: [
                        UtopiaTheme.layout.inspectorSmallWidth,
                        UtopiaTheme.layout.inspectorLargeWidth,
                      ],
                    },
                    enable: {
                      left: true,
                    },
                  }}
                />
              </div>,
            )}
            {when(draggablePanelsEnabled, <FloatingPanelsContainer />)}
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
  width: number
  height: number
  onResize: (menuName: 'inspector', direction: Direction, width: number, height: number) => void
  setIsResizing: React.Dispatch<React.SetStateAction<Menu | Pane | null>>
  resizableConfig: ResizableProps
}

export const ResizableRightPane = React.memo<ResizableRightPaneProps>((props) => {
  const { onResize: onPanelResize, setIsResizing, width, height } = props
  const colorTheme = useColorTheme()
  const [, updateInspectorWidth] = useAtom(InspectorWidthAtom)

  const [widthLocal, setWidthLocal] = React.useState<number>(UtopiaTheme.layout.inspectorSmallWidth)

  const resizableRef = React.useRef<Resizable>(null)
  const onResizeStart = React.useCallback(() => {
    setIsResizing('inspector')
  }, [setIsResizing])
  const onResize = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: Size,
    ) => {
      const newWidth = resizableRef.current?.size.width
      if (newWidth != null) {
        // we have to use the instance ref to directly access the get size() getter, because re-resize's API only wants to tell us deltas, but we need the snapped width
        if (isFeatureEnabled('Draggable Floating Panels')) {
          onPanelResize('inspector', direction, newWidth, elementRef?.clientHeight)
        } else {
          setWidthLocal(newWidth)
        }
        updateInspectorWidth(newWidth > UtopiaTheme.layout.inspectorSmallWidth ? 'wide' : 'regular')
      }
    },
    [updateInspectorWidth, onPanelResize],
  )
  const onResizeStop = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: Size,
    ) => {
      setIsResizing(null)
      onResize(event, direction, elementRef, delta)
    },
    [setIsResizing, onResize],
  )

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
        width: isFeatureEnabled('Draggable Floating Panels') ? width : widthLocal,
        height: isFeatureEnabled('Draggable Floating Panels') ? height : '100%',
      }}
      style={{
        transition: 'width 100ms ease-in-out',
        overflow: 'hidden',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
      }}
      onResizeStart={onResizeStart}
      onResize={onResize}
      onResizeStop={onResizeStop}
      {...props.resizableConfig}
    >
      {when(isFeatureEnabled('Draggable Floating Panels'), <TitleBarUserProfile />)}
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
  width: number
  height: number
  onResize: (menuName: 'code-editor', direction: Direction, width: number, height: number) => void
  setIsResizing: React.Dispatch<React.SetStateAction<Menu | Pane | null>>
  resizableConfig: ResizableProps
}

export const CodeEditorPane = React.memo<CodeEditorPaneProps>((props) => {
  const { width, height, onResize: onPanelResize, setIsResizing, resizableConfig } = props
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()
  const interfaceDesigner = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.interfaceDesigner,
    'CodeEditorPane interfaceDesigner',
  )

  const codeEditorEnabled = isCodeEditorEnabled()
  const onResizeStart = React.useCallback(() => {
    if (isFeatureEnabled('Draggable Floating Panels')) {
      setIsResizing('code-editor')
    }
  }, [setIsResizing])
  const onResizeStop = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: NumberSize,
    ) => {
      dispatch([EditorActions.resizeInterfaceDesignerCodePane(delta.width)])
      const newWidth = elementRef?.clientWidth
      const newHeight = elementRef?.clientHeight

      if (isFeatureEnabled('Draggable Floating Panels')) {
        onPanelResize('code-editor', direction, newWidth, newHeight)
        setIsResizing(null)
      }
    },
    [dispatch, onPanelResize, setIsResizing],
  )
  const onResize = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: NumberSize,
    ) => {
      const newWidth = elementRef?.clientWidth
      const newHeight = elementRef?.clientHeight
      if (newWidth != null && newHeight != null) {
        onPanelResize('code-editor', direction, newWidth, newHeight)
      }
    },
    [onPanelResize],
  )

  return (
    <Resizable
      defaultSize={{
        width: interfaceDesigner.codePaneWidth,
        height: '100%',
      }}
      size={{
        width: isFeatureEnabled('Draggable Floating Panels')
          ? width
          : interfaceDesigner.codePaneWidth,
        height: isFeatureEnabled('Draggable Floating Panels') ? height : '100%',
      }}
      onResizeStart={onResizeStart}
      onResizeStop={onResizeStop}
      onResize={onResize}
      className='resizableFlexColumnCanvasCode'
      style={{
        display: interfaceDesigner.codePaneVisible ? 'flex' : 'none',
        width: isFeatureEnabled('Draggable Floating Panels')
          ? interfaceDesigner.codePaneWidth
          : undefined,
        position: 'relative',
        overflow: 'hidden',
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
        flexDirection: 'column',
      }}
      {...resizableConfig}
    >
      {when(isFeatureEnabled('Draggable Floating Panels'), <TitleBarEmpty />)}
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
            height: props.small ? (height - TitleHeight) / 0.7 - 32 : '100%',
          }}
        >
          {when(codeEditorEnabled, <CodeEditorWrapper />)}
        </div>
        <ConsoleAndErrorsPane />
      </div>
    </Resizable>
  )
})
