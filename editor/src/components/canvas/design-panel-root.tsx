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
import { SimpleFlexRow, UtopiaTheme, UtopiaStyles } from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'

interface DesignPanelRootProps {
  isUiJsFileOpen: boolean
  runtimeErrors: Array<RuntimeErrorInfo>
  onRuntimeError: (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => void
  clearRuntimeErrors: () => void
  canvasConsoleLogs: Array<ConsoleLog>
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
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
  const navigatorVisible = useEditorState(
    (store) => store.editor.navigator.visible,
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
    },
    [updateDeltaWidth, props.isUiJsFileOpen],
  )

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
        <Resizable
          defaultSize={{ width: interfaceDesigner.codePaneWidth, height: '100%' }}
          size={props.isUiJsFileOpen ? undefined : { width: '100%', height: '100%' }} // this hack practically disables the Resizable without having to re-mount the code editor iframe
          onResizeStop={onResizeStop}
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
        {props.isUiJsFileOpen ? (
          <SimpleFlexRow
            style={{
              position: 'relative',
              overflowX: 'hidden',
              justifyContent: 'stretch',
              alignItems: 'stretch',
              flexGrow: 1,
            }}
          >
            <CanvasWrapperComponent {...props} />
            {navigatorVisible && (
              <NavigatorComponent
                style={{
                  position: 'absolute',
                  height: '100%',
                  left:
                    leftMenuExpanded && !interfaceDesigner.codePaneVisible
                      ? LeftPaneDefaultWidth
                      : undefined,
                  width: LeftPaneDefaultWidth,
                }}
              />
            )}
          </SimpleFlexRow>
        ) : null}
      </SimpleFlexRow>
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
