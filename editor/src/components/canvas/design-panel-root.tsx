import { Resizable, ResizeCallback, ResizeDirection } from 're-resizable'
import React from 'react'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import * as EditorActions from '../editor/actions/action-creators'

import {
  ConsoleLog,
  LeftPaneDefaultWidth,
  RightMenuTab,
  NavigatorWidthAtom,
} from '../editor/store/editor-state'

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
  ResizableFlexColumn,
} from '../../uuiui'

import { ConsoleAndErrorsPane } from '../code-editor/console-and-errors-pane'
import { FloatingInsertMenu } from './ui/floating-insert-menu'
import { usePubSubAtom } from '../../core/shared/atom-with-pub-sub'
import CanvasActions from './canvas-actions'
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

export const DesignPanelRoot = React.memo(() => {
  const dispatch = useDispatch()
  const interfaceDesigner = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.interfaceDesigner,
    'DesignPanelRoot interfaceDesigner',
  )

  const colorTheme = useColorTheme()
  const [codeEditorResizingWidth, setCodeEditorResizingWidth] = React.useState<number | null>(
    interfaceDesigner.codePaneWidth,
  )
  const navigatorVisible = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.navigator.minimised,
    'DesignPanelRoot navigatorVisible',
  )

  const isRightMenuExpanded = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.expanded,
    'DesignPanelRoot isRightMenuExpanded',
  )

  const rightMenuSelectedTab = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.selectedTab,
    'DesignPanelRoot rightMenuSelectedTab',
  )

  const leftMenuExpanded = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.leftMenu.expanded,
    'EditorComponentInner leftMenuExpanded',
  )

  const isCanvasVisible = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.visible,
    'design panel root',
  )

  const codeEditorEnabled = isCodeEditorEnabled()

  const isInsertMenuSelected = rightMenuSelectedTab === RightMenuTab.Insert

  const updateDeltaWidth = React.useCallback(
    (deltaWidth: number) => {
      dispatch([EditorActions.resizeInterfaceDesignerCodePane(deltaWidth)])
    },
    [dispatch],
  )

  const onResizeStop = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: NumberSize,
    ) => {
      updateDeltaWidth(delta.width)
    },
    [updateDeltaWidth],
  )

  const onResize = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: NumberSize,
    ) => {
      if (navigatorVisible) {
        setCodeEditorResizingWidth(interfaceDesigner.codePaneWidth + delta.width)
      }
    },
    [interfaceDesigner, navigatorVisible],
  )

  const [navigatorWidth, setNavigatorWidth] = usePubSubAtom(NavigatorWidthAtom)

  const onNavigatorResizeStop = React.useCallback<ResizeCallback>(
    (_event, _direction, _ref, delta) => {
      setNavigatorWidth((currentWidth) => currentWidth + delta.width)
      dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: -delta.width, y: 0 }))])
    },
    [setNavigatorWidth, dispatch],
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
        {!isCanvasVisible && !interfaceDesigner.codePaneVisible ? (
          <div
            style={{
              width: '100%',
              height: '100%',
              position: 'absolute',
              left: 0,
              right: 0,
              bottom: 0,
              top: 0,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <NothingOpenCard />
          </div>
        ) : null}

        {/* code editor */}
        <Resizable
          defaultSize={{
            width: isCanvasVisible ? interfaceDesigner.codePaneWidth : '100%',
            height: '90%',
          }}
          size={{
            width: isCanvasVisible ? interfaceDesigner.codePaneWidth : '100%',
            height: '90%',
          }}
          onResizeStop={onResizeStop}
          onResize={onResize}
          enable={{
            top: false,
            right: isCanvasVisible,
            bottom: false,
            topRight: false,
            bottomRight: false,
            bottomLeft: false,
            topLeft: false,
          }}
          className='resizableFlexColumnCanvasCode'
          style={{
            ...UtopiaStyles.flexColumn,
            display: interfaceDesigner.codePaneVisible ? 'flex' : 'none',
            width: isCanvasVisible ? undefined : interfaceDesigner.codePaneWidth,
            overflow: 'hidden',
            justifyContent: 'stretch',
            alignItems: 'stretch',
            margin: 10,
            borderRadius: 10,
            boxShadow: '3px 4px 10px 0px rgba(0,0,0, .3)',
            position: 'absolute',
            left: 0,
            top: 0,
            zIndex: 100,
            height: '90%',
          }}
        >
          {when(codeEditorEnabled, <CodeEditorWrapper />)}
          <ConsoleAndErrorsPane />
        </Resizable>

        {/* left pane */}
        {leftMenuExpanded ? <LeftPaneComponent /> : null}

        {isCanvasVisible ? (
          <SimpleFlexColumn
            style={{
              flexGrow: 1,
              overflow: 'hidden',
              position: 'relative',
            }}
          >
            {isCanvasVisible && navigatorVisible ? (
              <div
                style={{
                  height: '100%',
                  position: 'absolute',
                  top: 0,
                  left: 510,
                  zIndex: 20,
                  overflow: 'hidden',
                }}
              >
                {/* navigator */}
                <ResizableFlexColumn
                  style={{
                    overscrollBehavior: 'contain',
                    backgroundColor: colorTheme.inspectorBackground.value,
                    height: '90%',
                    margin: 10,
                    borderRadius: 10,
                    overflow: 'scroll',
                    boxShadow: '3px 4px 10px 0px rgba(0,0,0, .3)',
                  }}
                  onResizeStop={onNavigatorResizeStop}
                  defaultSize={{
                    width: navigatorWidth,
                    height: '90%',
                  }}
                >
                  <NavigatorComponent />
                </ResizableFlexColumn>
              </div>
            ) : null}

            <CanvasWrapperComponent />

            {/* toolbar */}
            <FloatingInsertMenu />
          </SimpleFlexColumn>
        ) : null}
      </SimpleFlexRow>

      {isCanvasVisible ? (
        <>
          {isRightMenuExpanded ? (
            <ResizableInspectorPane isInsertMenuSelected={isInsertMenuSelected} />
          ) : null}
        </>
      ) : null}
    </>
  )
})

export const DesignPanelRootOld = React.memo(() => {
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
        {/* <DesignPanelRootInner /> */}
      </SimpleFlexRow>
    </>
  )
})
DesignPanelRoot.displayName = 'DesignPanelRoot'

interface ResizableInspectorPaneProps {
  isInsertMenuSelected: boolean
}
const ResizableInspectorPane = React.memo<ResizableInspectorPaneProps>((props) => {
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

  return (
    <Resizable
      ref={resizableRef}
      defaultSize={{
        width: UtopiaTheme.layout.inspectorSmallWidth,
        height: '90%',
      }}
      size={{
        width: width,
        height: '90%',
      }}
      style={{
        transition: 'width 100ms ease-in-out',
        height: '95%',
        position: 'absolute',
        right: 0,
        margin: 10,
        borderRadius: 10,
        overflow: 'scroll',
        backgroundColor: colorTheme.inspectorBackground.value,
        boxShadow: '3px 4px 10px 0px rgba(0,0,0, .3)',
      }}
      snap={{
        x: [UtopiaTheme.layout.inspectorSmallWidth, UtopiaTheme.layout.inspectorLargeWidth],
      }}
      onResizeStart={onResize}
      onResize={onResize}
      onResizeStop={onResize}
      enable={{
        left: true,
      }}
    >
      <SimpleFlexRow
        className='Inspector-entrypoint'
        style={{
          alignItems: 'stretch',
          flexDirection: 'column',
          width: '100%',
          height: '100%',
          overflowY: 'scroll',

          flexGrow: 0,
          flexShrink: 0,
          paddingBottom: props.isInsertMenuSelected ? 0 : 100,
        }}
      >
        {props.isInsertMenuSelected ? <InsertMenuPane /> : <InspectorEntryPoint />}
      </SimpleFlexRow>
      <CanvasStrategyInspector />
    </Resizable>
  )
})
