import { Resizable, ResizeDirection } from 're-resizable'
import * as React from 'react'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import * as EditorActions from '../editor/actions/action-creators'

import { ConsoleLog, LeftPaneDefaultWidth, RightMenuTab } from '../editor/store/editor-state'

import { useEditorState } from '../editor/store/store-hook'
import { InspectorEntryPoint } from '../inspector/inspector'
import { CanvasWrapperComponent } from './canvas-wrapper-component'
import { InsertMenuPane } from '../navigator/left-pane'

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
import { betterReactMemo } from '../../uuiui-deps'
import { TopMenu } from '../editor/top-menu'
import { ConsoleAndErrorsPane } from '../code-editor/console-and-errors-pane'
import { FloatingInsertMenu } from './ui/floating-insert-menu'

interface DesignPanelRootProps {
  isUiJsFileOpen: boolean
}

interface NumberSize {
  width: number
  height: number
}

const TopMenuHeight = 34

const NothingOpenCard = betterReactMemo('NothingOpen', () => {
  const colorTheme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'NothingOpenCard dispatch')
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

export const DesignPanelRoot = betterReactMemo('DesignPanelRoot', (props: DesignPanelRootProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'DesignPanelRoot dispatch')
  const interfaceDesigner = useEditorState(
    (store) => store.editor.interfaceDesigner,
    'DesignPanelRoot interfaceDesigner',
  )

  const colorTheme = useColorTheme()
  const [codeEditorResizingWidth, setCodeEditorResizingWidth] = React.useState<number | null>(
    interfaceDesigner.codePaneWidth,
  )
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'DesignPanelRoot navigatorVisible',
  )

  const isRightMenuExpanded = useEditorState(
    (store) => store.editor.rightMenu.expanded,
    'DesignPanelRoot isRightMenuExpanded',
  )

  const rightMenuSelectedTab = useEditorState(
    (store) => store.editor.rightMenu.selectedTab,
    'DesignPanelRoot rightMenuSelectedTab',
  )

  const leftMenuExpanded = useEditorState(
    (store) => store.editor.leftMenu.expanded,
    'EditorComponentInner leftMenuExpanded',
  )

  const isCanvasVisible = useEditorState(
    (store) => store.editor.canvas.visible,
    'design panel root',
  )

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
      if (props.isUiJsFileOpen) {
        updateDeltaWidth(delta.width)
      }
      setCodeEditorResizingWidth(null)
    },
    [updateDeltaWidth, props.isUiJsFileOpen],
  )

  const onResize = React.useCallback(
    (
      event: MouseEvent | TouchEvent,
      direction: ResizeDirection,
      elementRef: HTMLElement,
      delta: NumberSize,
    ) => {
      if (props.isUiJsFileOpen && navigatorVisible) {
        setCodeEditorResizingWidth(interfaceDesigner.codePaneWidth + delta.width)
      }
    },
    [interfaceDesigner, navigatorVisible, props.isUiJsFileOpen],
  )

  const getNavigatorLeft = React.useMemo((): number | undefined => {
    const codeEditorCurrentWidth =
      codeEditorResizingWidth != null ? codeEditorResizingWidth : interfaceDesigner.codePaneWidth
    let position = navigatorVisible ? codeEditorCurrentWidth : undefined

    if (!interfaceDesigner.codePaneVisible) {
      if (leftMenuExpanded) {
        position = LeftPaneDefaultWidth
      } else {
        position = undefined
      }
    }
    return position
  }, [
    codeEditorResizingWidth,
    interfaceDesigner.codePaneVisible,
    interfaceDesigner.codePaneWidth,
    leftMenuExpanded,
    navigatorVisible,
  ])

  return (
    <SimpleFlexRow
      className='OpenFileEditorShell'
      style={{
        position: 'relative',
        flexGrow: 1,
        alignItems: 'stretch',
        overflowX: 'hidden',
      }}
    >
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
        <SimpleFlexColumn style={{ flexGrow: isCanvasVisible ? undefined : 1 }}>
          <Resizable
            defaultSize={{
              width: isCanvasVisible ? interfaceDesigner.codePaneWidth : '100%',
              height: '100%',
            }}
            size={{
              width: isCanvasVisible ? interfaceDesigner.codePaneWidth : '100%',
              height: '100%',
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
              display: !props.isUiJsFileOpen || interfaceDesigner.codePaneVisible ? 'flex' : 'none',
              width: isCanvasVisible ? undefined : interfaceDesigner.codePaneWidth,
              height: '100%',
              position: 'relative',
              overflow: 'hidden',
              justifyContent: 'stretch',
              alignItems: 'stretch',
              borderLeft: `1px solid ${colorTheme.subduedBorder.value}`,
            }}
          >
            <CodeEditorWrapper />
            <ConsoleAndErrorsPane />
          </Resizable>
        </SimpleFlexColumn>

        {props.isUiJsFileOpen && isCanvasVisible ? (
          <SimpleFlexColumn
            style={{
              flexGrow: 1,
              overflow: 'hidden',
              position: 'relative',
            }}
          >
            <SimpleFlexRow
              className='topMenu'
              style={{
                minHeight: TopMenuHeight,
                height: TopMenuHeight,
                borderBottom: `1px solid ${colorTheme.border0.value}`,
                alignItems: 'stretch',
                justifyContent: 'stretch',
                backgroundColor: 'transparent',
              }}
            >
              <TopMenu />
            </SimpleFlexRow>

            {isCanvasVisible && props.isUiJsFileOpen && navigatorVisible ? (
              <div
                style={{
                  height: `calc(100% - ${TopMenuHeight}px)`,
                  position: 'absolute',
                  top: TopMenuHeight,
                  left: 0,
                  zIndex: 20,
                  overflow: 'hidden',
                }}
              >
                <ResizableFlexColumn
                  style={{
                    overscrollBehavior: 'contain',
                    backgroundColor: UtopiaTheme.color.bg0.o(90).value,
                    backdropFilter: 'blur(7px)',
                  }}
                  defaultSize={{
                    width: 280,
                    height: '100%',
                  }}
                >
                  <NavigatorComponent
                    style={{
                      zIndex: 1,
                      flexGrow: 1,
                      height: '100%',
                      display: 'flex',
                      flexDirection: 'column',
                      alignItems: 'stretch',
                      justifyContent: 'stretch',
                      overscrollBehavior: 'contain',
                    }}
                  />
                </ResizableFlexColumn>
              </div>
            ) : null}
            <CanvasWrapperComponent {...props} />
            <FloatingInsertMenu />
          </SimpleFlexColumn>
        ) : null}
      </SimpleFlexRow>

      {isCanvasVisible && props.isUiJsFileOpen ? (
        <>
          {isRightMenuExpanded ? (
            <SimpleFlexRow
              className='Inspector-entrypoint'
              style={{
                alignItems: 'stretch',
                flexDirection: 'column',
                width: UtopiaTheme.layout.inspectorWidth,
                backgroundColor: colorTheme.inspectorBackground.value,
                flexGrow: 0,
                flexShrink: 0,
                overflowY: 'scroll',
                paddingBottom: 100,
              }}
            >
              {isInsertMenuSelected ? <InsertMenuPane /> : <InspectorEntryPoint />}
            </SimpleFlexRow>
          ) : null}
        </>
      ) : null}
    </SimpleFlexRow>
  )
})
DesignPanelRoot.displayName = 'DesignPanelRoot'
