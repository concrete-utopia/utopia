import { Resizable, ResizeDirection } from 're-resizable'
import * as React from 'react'
import { SimpleFlexRow, UtopiaStyles, UtopiaTheme } from 'uuiui'

import { betterReactMemo } from 'uuiui-deps'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { ScriptEditor } from '../code-editor/script-editor'
import * as EditorActions from '../editor/actions/actions'

import { ConsoleLog } from '../editor/store/editor-state'

import { useEditorState } from '../editor/store/store-hook'
import { InspectorEntryPoint } from '../inspector/inspector'
import { CanvasWrapperComponent } from './canvas-wrapper-component'
import { InsertMenuPane } from '../navigator/left-pane'

import { RightMenu, RightMenuTab } from './right-menu'

interface SplitViewCanvasRootProps {
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

export const SplitViewCanvasRoot = betterReactMemo(
  'SplitViewCanvasRoot',
  (props: SplitViewCanvasRootProps) => {
    const dispatch = useEditorState((store) => store.dispatch, 'SplitViewCanvasRoot dispatch')
    const interfaceDesigner = useEditorState(
      (store) => store.editor.interfaceDesigner,
      'SplitViewCanvasRoot interfaceDesigner',
    )
    const layoutReversed = interfaceDesigner.layoutReversed

    const isRightMenuExpanded = useEditorState(
      (store) => store.editor.rightMenu.expanded,
      'SplitViewCanvasRoot isRightMenuExpanded',
    )

    const rightMenuSelectedTab = useEditorState(
      (store) => store.editor.rightMenu.selectedTab,
      'SplitViewCanvasRoot rightMenuSelectedTab',
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
        updateDeltaWidth(delta.width)
      },
      [updateDeltaWidth],
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
            flexDirection: layoutReversed ? 'row-reverse' : 'row',
            alignItems: 'stretch',
            overflowX: 'hidden',
            flexGrow: 1,
            flexShrink: 0,
            borderRight: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
          }}
        >
          <CanvasWrapperComponent {...props} />
          {interfaceDesigner.codePaneVisible ? (
            <Resizable
              defaultSize={{ width: interfaceDesigner.codePaneWidth, height: '100%' }}
              onResizeStop={onResizeStop}
              enable={{
                top: false,
                right: layoutReversed,
                bottom: false,
                left: !layoutReversed,
                topRight: false,
                bottomRight: false,
                bottomLeft: false,
                topLeft: false,
              }}
              className='resizableFlexColumnCanvasCode'
              style={{
                ...UtopiaStyles.flexColumn,
                width: interfaceDesigner.codePaneWidth,
                height: '100%',
                position: 'relative',
                overflow: 'hidden',
                justifyContent: 'stretch',
                alignItems: 'stretch',
                borderLeft: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
              }}
            >
              <ScriptEditor
                relevantPanel={'uicodeeditor'}
                runtimeErrors={props.runtimeErrors}
                canvasConsoleLogs={props.canvasConsoleLogs}
              />
            </Resizable>
          ) : null}
        </SimpleFlexRow>
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
      </SimpleFlexRow>
    )
  },
)
SplitViewCanvasRoot.displayName = 'SplitViewCanvasRoot'
