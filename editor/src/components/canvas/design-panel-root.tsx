import type { ResizeCallback, ResizeDirection } from 're-resizable'
import type { Resizable } from 're-resizable'
import React from 'react'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import * as EditorActions from '../editor/actions/action-creators'

import { CanvasSizeAtom, ConsoleLog, RightMenuTab } from '../editor/store/editor-state'

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
import type ReactGridLayout from 'react-grid-layout'
import { Responsive as ResponsiveGridLayout } from 'react-grid-layout'
import 'react-grid-layout/css/styles.css'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'

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

const MaxGridHeight = 10
const DesignPanelRootInner = React.memo(() => {
  const [layout, setLayout] = React.useState<Array<ReactGridLayout.Layout>>([
    {
      i: 'leftpane',
      x: 0,
      y: 0,
      w: 2,
      h: 10,
      minW: 2,
      maxW: 6,
      minH: 1,
      maxH: 10,
      resizeHandles: ['s', 'e', 'se', 'sw'],
    },
    {
      i: 'codeeditor',
      x: 2,
      y: 0,
      w: 4,
      h: 10,
      minW: 2,
      maxW: 4,
      minH: 1,
      maxH: 10,
      resizeHandles: ['s', 'e', 'se', 'sw'],
    },
    {
      i: 'rightpane',
      x: 10,
      y: 0,
      w: 2,
      h: 10,
      minW: 2,
      maxW: 6,
      minH: 1,
      maxH: 10,
      resizeHandles: ['s', 'w', 'se', 'sw'],
    },
  ])
  const lastLayoutWithMaxHeight = React.useRef<Array<ReactGridLayout.Layout>>([])
  const updateLastLayoutBeforeDrag = React.useCallback(
    (latestLayout: Array<ReactGridLayout.Layout>) => {
      lastLayoutWithMaxHeight.current = latestLayout
    },
    [],
  )
  const updateNewLayoutIfItsOutOfScreen = React.useCallback(
    (currentLayout: Array<ReactGridLayout.Layout> | null) => {
      if (currentLayout == null) {
        return false
      }
      const updatedLayout = currentLayout.reduce((working, current, i) => {
        if (current.y + current.h > MaxGridHeight) {
          const prev = working[i - 1]
          if (prev != null && prev.h !== currentLayout[i - 1].h) {
            return [
              ...working,
              {
                ...current,
                y: current.y - (currentLayout[i - 1].h - prev.h),
              },
            ]
          }
          return [
            ...working,
            {
              ...current,
              h: Math.max(0, MaxGridHeight - current.y),
            },
          ]
        }
        return [...working, current]
      }, [] as ReactGridLayout.Layout[])

      if (updatedLayout.some((updated, i) => updated.h !== currentLayout[i].h)) {
        setLayout(updatedLayout)
        return true
      }
      return false
    },
    [setLayout],
  )

  const onLayoutChange = React.useCallback(
    (newLayout: Array<ReactGridLayout.Layout>) => {
      setLayout(newLayout)
      updateNewLayoutIfItsOutOfScreen(newLayout)
    },
    [updateNewLayoutIfItsOutOfScreen, setLayout],
  )

  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)
  const gridSize = 12

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
        <SimpleFlexColumn
          style={{
            flexGrow: 1,
            overflow: 'hidden',
            position: 'relative',
          }}
        >
          <CanvasWrapperComponent />
        </SimpleFlexColumn>
        <ResponsiveGridLayout
          style={{ position: 'absolute', zIndex: 1 }}
          className='layout'
          rowHeight={canvasSize.height / gridSize} // this seems to be wrong
          width={canvasSize.width}
          margin={[10, 10]}
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          layouts={{ lg: layout }} // something is wrong here
          cols={{ lg: gridSize, md: gridSize, sm: gridSize, xs: gridSize, xxs: gridSize }}
          onLayoutChange={onLayoutChange}
          onDragStart={updateLastLayoutBeforeDrag}
          onResizeStart={updateLastLayoutBeforeDrag}
        >
          <div
            key='leftpane'
            // style={{ outline: '1px solid red' }}
          >
            <LeftPaneComponent />
          </div>
          <div
            key='codeeditor'
            // style={{ outline: '1px solid red' }}
          >
            <CodeEditorPane />
          </div>
          <div
            key='rightpane'
            // style={{ outline: '1px solid red' }}
          >
            <ResizableRightPane />
          </div>
        </ResponsiveGridLayout>
      </SimpleFlexRow>
      <FloatingInsertMenu />
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

const ResizableRightPane = React.memo(() => {
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
    <div
      id='inspector-root'
      style={{
        // height: 'calc(100% - 20px)',
        // position: 'absolute',
        // right: 0,
        // margin: 10,
        width: '100%',
        height: '100%',
        overflow: 'hidden',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
      }}
    >
      {/* <Resizable
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
        enable={{
          left: true,
        }}
      > */}
      <SimpleFlexRow
        className='Inspector-entrypoint'
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
      {/* </Resizable> */}
    </div>
  )
})

const CodeEditorPane = React.memo(() => {
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
      style={{
        // height: 'calc(100% - 20px)',
        // position: 'absolute',
        // margin: 10,
        // zIndex: 1,
        display: 'flex',
        flexDirection: 'column',
        width: '100%',
        height: '100%',
        overflow: 'hidden',
        backgroundColor: colorTheme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
      }}
    >
      {/* <Resizable
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
          ...UtopiaStyles.flexColumn,
          display: interfaceDesigner.codePaneVisible ? 'flex' : 'none',
          width: interfaceDesigner.codePaneWidth,
          height: '100%',
          position: 'relative',
          overflow: 'hidden',
          justifyContent: 'stretch',
          alignItems: 'stretch',
          borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
          boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
        }}
      > */}
      {when(codeEditorEnabled, <CodeEditorWrapper />)}
      <ConsoleAndErrorsPane />
      {/* </Resizable> */}
    </div>
  )
})
