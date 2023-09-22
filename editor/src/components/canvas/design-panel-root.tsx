import type { Resizable, ResizeCallback, ResizeDirection } from 're-resizable'
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
import { GridMenuWidth } from './grid-panels-state'
import { GridPanelsContainer } from './grid-panels-container'
import type { Menu, Pane, StoredPanel } from './grid-panels-state'
import type { ResizableProps } from '../../uuiui-deps'
import type { Direction } from 're-resizable/lib/resizer'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { NO_OP } from '../../core/shared/utils'
import { TitleBarEmpty, TitleBarUserProfile, TitleHeight } from '../titlebar/title-bar'
import { useGridPanelDraggable } from './grid-panels-dnd'

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
                <CodeEditorPane panelData={null as any} small={false} />
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
                <LeftPaneComponent panelData={null as any} />
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
                <ResizableRightPane panelData={null as any} />
              </div>,
            )}
            {when(draggablePanelsEnabled, <GridPanelsContainer />)}
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
  const { drag, dragPreview } = useGridPanelDraggable(props.panelData)
  const defaultInspectorWidth = isFeatureEnabled('Draggable Floating Panels')
    ? GridMenuWidth
    : UtopiaTheme.layout.inspectorSmallWidth

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
  if (!isRightMenuExpanded) {
    return null
  }

  return (
    <div
      ref={dragPreview}
      style={{
        transition: 'width 100ms ease-in-out',
        overflow: 'hidden',
        width: '100%',
        height: '100%',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
      }}
    >
      {when(isFeatureEnabled('Draggable Floating Panels'), <TitleBarUserProfile ref={drag} />)}
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
    </div>
  )
})

interface CodeEditorPaneProps {
  panelData: StoredPanel
  small: boolean
}

export const CodeEditorPane = React.memo<CodeEditorPaneProps>((props) => {
  const { drag, dragPreview } = useGridPanelDraggable(props.panelData)
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
    <div
      ref={dragPreview}
      className='resizableFlexColumnCanvasCode'
      style={{
        display: props.small ? 'block' : interfaceDesigner.codePaneVisible ? 'flex' : 'none',
        width: '100%',
        height: '100%',
        position: 'relative',
        overflow: 'hidden',
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        flexDirection: 'column',
      }}
    >
      {when(isFeatureEnabled('Draggable Floating Panels'), <TitleBarEmpty ref={drag} />)}
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
    </div>
  )
})
