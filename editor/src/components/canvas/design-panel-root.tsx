import { Resizable, ResizeDirection } from 're-resizable'
import * as React from 'react'

import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import * as EditorActions from '../editor/actions/action-creators'

import { ConsoleLog } from '../editor/store/editor-state'

import { useEditorState } from '../editor/store/store-hook'
import { InspectorEntryPoint } from '../inspector/inspector'
import { CanvasWrapperComponent } from './canvas-wrapper-component'
import { InsertMenuPane, LeftPaneDefaultWidth } from '../navigator/left-pane'

import { RightMenu, RightMenuTab } from './right-menu'
import { CodeEditorWrapper } from '../code-editor/code-editor-container'
import { NavigatorComponent } from '../navigator/navigator'
import { SimpleFlexRow, UtopiaTheme, UtopiaStyles, SimpleFlexColumn } from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { FileTabs, TopMenuHeight } from '../filebrowser/file-tabs'
import { TopMenu } from '../editor/top-menu'

interface DesignPanelRootProps {
  isUiJsFileOpen: boolean
}

interface NumberSize {
  width: number
  height: number
}

export const DesignPanelRoot = betterReactMemo('DesignPanelRoot', (props: DesignPanelRootProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'DesignPanelRoot dispatch')
  const interfaceDesigner = useEditorState(
    (store) => store.editor.interfaceDesigner,
    'DesignPanelRoot interfaceDesigner',
  )
  const [codeEditorResizingWidth, setCodeEditorResizingWidth] = React.useState<number | null>(
    interfaceDesigner.codePaneWidth,
  )
  const navigatorPosition = useEditorState(
    (store) => store.editor.navigator.position,
    'DesignPanelRoot navigatorPosition',
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
      elementRef: HTMLDivElement,
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
      elementRef: HTMLDivElement,
      delta: NumberSize,
    ) => {
      if (props.isUiJsFileOpen && navigatorPosition !== 'hidden') {
        setCodeEditorResizingWidth(interfaceDesigner.codePaneWidth + delta.width)
      }
    },
    [interfaceDesigner, navigatorPosition, props.isUiJsFileOpen],
  )

  const getNavigatorLeft = React.useMemo((): number | undefined => {
    let position = undefined
    const codeEditorCurrentWidth =
      codeEditorResizingWidth != null ? codeEditorResizingWidth : interfaceDesigner.codePaneWidth
    switch (navigatorPosition) {
      case 'left':
        position = codeEditorCurrentWidth - LeftPaneDefaultWidth
        break
      case 'right':
        position = codeEditorCurrentWidth
        break
    }
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
    navigatorPosition,
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
          borderRight: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
        }}
      >
        <SimpleFlexColumn style={{ flexGrow: props.isUiJsFileOpen ? undefined : 1 }}>
          <FileTabs />
          <Resizable
            defaultSize={{ width: interfaceDesigner.codePaneWidth, height: '100%' }}
            size={{
              width: props.isUiJsFileOpen ? interfaceDesigner.codePaneWidth : '100%',
              height: '100%',
            }}
            onResizeStop={onResizeStop}
            onResize={onResize}
            enable={{
              top: false,
              right: props.isUiJsFileOpen,
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
              width: interfaceDesigner.codePaneWidth,
              height: '100%',
              position: 'relative',
              overflow: 'hidden',
              justifyContent: 'stretch',
              alignItems: 'stretch',
              borderLeft: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
            }}
          >
            <CodeEditorWrapper />
          </Resizable>
        </SimpleFlexColumn>
        {props.isUiJsFileOpen ? (
          <SimpleFlexColumn style={{ flexGrow: 1 }}>
            <SimpleFlexRow
              className='topMenu'
              style={{
                minHeight: TopMenuHeight,
                height: TopMenuHeight,
                borderBottom: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
                alignItems: 'stretch',
                justifyContent: 'stretch',
                backgroundColor: 'transparent',
              }}
            >
              <TopMenu />
            </SimpleFlexRow>
            <CanvasWrapperComponent {...props} />
          </SimpleFlexColumn>
        ) : null}
      </SimpleFlexRow>
      {props.isUiJsFileOpen && navigatorPosition !== 'hidden' ? (
        <NavigatorComponent
          style={{
            position: 'absolute',
            top: 30,
            height: 'calc(100% - 30px)',
            left: getNavigatorLeft,
            width: LeftPaneDefaultWidth,
          }}
        />
      ) : null}
      {props.isUiJsFileOpen ? (
        <>
          <RightMenu visible={true} />
          {isRightMenuExpanded ? (
            <SimpleFlexRow
              className='Inspector-entrypoint'
              style={{
                alignItems: 'stretch',
                flexDirection: 'column',
                width: UtopiaTheme.layout.inspectorWidth,
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
