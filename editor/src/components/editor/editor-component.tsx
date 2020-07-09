/** @jsx jsx */
import { jsx } from '@emotion/core'
import { ResizeDirection } from 're-resizable'
import * as React from 'react'
import { DndProvider } from 'react-dnd'
import Backend from 'react-dnd-html5-backend'
import {
  Button,
  colorTheme,
  FlexRow,
  LargerIcons,
  ResizableFlexColumn,
  SimpleFlexColumn,
  SimpleFlexRow,
  SquareButton,
  Subdued,
  TabComponent,
  UtopiaTheme,
  FlexColumn,
  UtopiaStyles,
} from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import Utils from '../../utils/utils'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { getCursorFromDragState } from '../canvas/canvas-utils'
import { SplitViewCanvasRoot } from '../canvas/split-view-canvas-root'
import { ScriptEditor } from '../code-editor/script-editor'
import { resizeLeftPane } from '../common/actions'
import { ConfirmCloseDialog } from '../filebrowser/confirm-close-dialog'
import { ConfirmDeleteDialog } from '../filebrowser/confirm-delete-dialog'
import { FileTabs } from '../filebrowser/file-tabs'
import { Menubar } from '../menubar/menubar'
import { LeftPaneComponent, LeftMenuTab } from '../navigator/left-pane'
import { PreviewColumn } from '../preview/preview-pane'
import { ReleaseNotesContent } from '../documentation/release-notes'
import { EditorDispatch, LoginState } from './action-types'
import * as EditorActions from './actions/actions'
import { handleKeyDown, handleKeyUp } from './editor-interactions'
import { StateHistory } from './history'
import { LoginStatusBar, EditorOfflineBar, BrowserInfoBar } from './notification-bar'
import {
  ConsoleLog,
  DerivedState,
  EditorState,
  getOpenEditorTab,
  getOpenFile,
  isReleaseNotesTab,
} from './store/editor-state'
import { useEditorState, useRefEditorState } from './store/store-hook'
import { isUIJSFile } from '../../core/shared/project-file-types'
import { isLiveMode, dragAndDropInsertionSubject, EditorModes, isSelectMode } from './editor-modes'
import { Toast } from '../common/notices'
import { chrome as isChrome } from 'platform-detect'

interface NumberSize {
  width: number
  height: number
}

export interface EditorProps {
  dispatch: EditorDispatch
  editorState: EditorState
  derivedState: DerivedState
  stateHistory: StateHistory
  loginState: LoginState
  tsWorker: Worker
}

const EmptyArray: Array<RuntimeErrorInfo> = []

const ConsoleLogSizeLimit = 100
let consoleLogTimeoutID: number | null = null
let canvasConsoleLogsVariable: Array<ConsoleLog> = []

export const EditorComponentInner = betterReactMemo('EditorComponentInner', () => {
  const editorStoreRef = useRefEditorState((store) => store)

  const onWindowMouseDown = React.useCallback(
    (event: MouseEvent) => {
      const popupId = editorStoreRef.current.editor.openPopupId
      if (popupId != null) {
        const popupElement = document.getElementById(popupId)
        const triggerElement = document.getElementById(`trigger-${popupId}`)
        const clickOutsidePopup =
          popupElement != null && !popupElement.contains(event.target as Node)
        const clickOutsideTrigger =
          triggerElement != null && !triggerElement.contains(event.target as Node)
        if (
          (clickOutsidePopup && triggerElement == null) ||
          (clickOutsidePopup && clickOutsideTrigger)
        ) {
          editorStoreRef.current.dispatch([EditorActions.closePopup()], 'everyone')
        }
      }
      const activeElement = document.activeElement
      if (
        event.target !== activeElement &&
        activeElement != null &&
        activeElement.getAttribute('data-inspector-input') &&
        (activeElement as any).blur != null
      ) {
        // OMG what a nightmare! This is the only way of keeping the Inspector fast and ensuring the blur handler for inputs
        // is called before triggering a change that might change the selection
        ;(activeElement as any).blur()
      }
    },
    [editorStoreRef],
  )

  const onWindowKeyDown = React.useCallback(
    (event: KeyboardEvent) => {
      handleKeyDown(
        event,
        editorStoreRef.current.editor,
        editorStoreRef.current.derived,
        editorStoreRef.current.dispatch,
      )
    },
    [editorStoreRef],
  )

  const onWindowKeyUp = React.useCallback(
    (event) => {
      handleKeyUp(
        event,
        editorStoreRef.current.editor,
        editorStoreRef.current.derived,
        editorStoreRef.current.dispatch,
      )
    },
    [editorStoreRef],
  )

  const preventDefault = React.useCallback((event: MouseEvent) => {
    event.preventDefault()
  }, [])

  React.useEffect(() => {
    window.addEventListener('mousedown', onWindowMouseDown, true)
    window.addEventListener('keydown', onWindowKeyDown)
    window.addEventListener('keyup', onWindowKeyUp)
    window.addEventListener('contextmenu', preventDefault)
    return function cleanup() {
      window.removeEventListener('mousedown', onWindowMouseDown)
      window.removeEventListener('keydown', onWindowKeyDown)
      window.removeEventListener('keyup', onWindowKeyUp)
      window.removeEventListener('contextmenu', preventDefault)
    }
  }, [onWindowMouseDown, onWindowKeyDown, onWindowKeyUp, preventDefault])

  const dispatch = useEditorState((store) => store.dispatch)
  const projectName = useEditorState((store) => store.editor.projectName)
  const previewVisible = useEditorState((store) => store.editor.preview.visible)
  const leftMenuExpanded = useEditorState((store) => store.editor.leftMenu.expanded)
  const leftMenuWidth = useEditorState((store) => store.editor.leftMenu.paneWidth)
  const saveError = useEditorState((store) => store.editor.saveError)

  React.useEffect(() => {
    document.title = projectName + ' - Utopia'
  }, [projectName])

  const onClosePreview = React.useCallback(
    () => dispatch([EditorActions.setPanelVisibility('preview', false)]),
    [dispatch],
  )

  const updateDeltaWidth = React.useCallback(
    (deltaWidth: number) => {
      dispatch([resizeLeftPane(deltaWidth)])
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

  const [runtimeErrors, setRuntimeErrors] = React.useState<Array<RuntimeErrorInfo>>(EmptyArray)

  const onRuntimeError = React.useCallback(
    (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => {
      setRuntimeErrors([
        {
          editedFile: editedFile,
          error: error,
          errorInfo: Utils.defaultIfNull(null, errorInfo),
        },
      ])
    },
    [],
  )

  const clearRuntimeErrors = React.useCallback(() => {
    setRuntimeErrors(EmptyArray)
  }, [])

  const [canvasConsoleLogsState, setCanvasConsoleLogsState] = React.useState<Array<ConsoleLog>>([])

  const modifyLogs = React.useCallback(
    (updateLogs: (logs: Array<ConsoleLog>) => Array<ConsoleLog>) => {
      canvasConsoleLogsVariable = updateLogs(canvasConsoleLogsVariable)
      if (consoleLogTimeoutID != null) {
        clearTimeout(consoleLogTimeoutID)
      }
      consoleLogTimeoutID = setTimeout(() => {
        setCanvasConsoleLogsState(canvasConsoleLogsVariable)
        consoleLogTimeoutID = null
      })
    },
    [setCanvasConsoleLogsState],
  )

  const clearConsoleLogs = React.useCallback(() => {
    modifyLogs((_) => [])
  }, [modifyLogs])

  const addToConsoleLogs = React.useCallback(
    (log: ConsoleLog) => {
      modifyLogs((logs) => {
        let result = [...logs, log]
        while (result.length > ConsoleLogSizeLimit) {
          result.shift()
        }
        return result
      })
    },
    [modifyLogs],
  )

  const canvasIsLive = useEditorState((store) => isLiveMode(store.editor.mode))

  const toggleLiveCanvas = React.useCallback(() => dispatch([EditorActions.toggleCanvasIsLive()]), [
    dispatch,
  ])

  const startDragInsertion = React.useCallback(
    (event: React.DragEvent<HTMLDivElement>) => {
      const draggedTypes = event.nativeEvent?.dataTransfer?.types
      const isDraggedFile =
        draggedTypes != null && draggedTypes.length === 1 && draggedTypes[0] === 'Files'
      const currentMode = editorStoreRef.current.editor.mode
      if (
        isSelectMode(currentMode) ||
        (isDraggedFile && isLiveMode(editorStoreRef.current.editor.mode))
      ) {
        const actions = [
          EditorActions.switchEditorMode(
            EditorModes.insertMode(false, dragAndDropInsertionSubject(null)),
          ),
          EditorActions.setPanelVisibility('leftmenu', true),
          EditorActions.setLeftMenuTab(LeftMenuTab.ProjectStructure),
        ]
        dispatch(actions, 'everyone')
      }
    },
    [dispatch, editorStoreRef],
  )

  return (
    <SimpleFlexRow
      className='editor-main-vertical-and-modals'
      style={{
        height: '100%',
        width: '100%',
        overscrollBehaviorX: 'contain',
      }}
      onDragEnter={startDragInsertion}
    >
      <SimpleFlexColumn
        className='editor-main-vertical'
        style={{
          height: '100%',
          width: '100%',
        }}
      >
        {isChrome ? null : <BrowserInfoBar />}
        <LoginStatusBar />
        {saveError ? <EditorOfflineBar /> : null}

        <SimpleFlexRow
          className='editor-main-horizontal'
          style={{
            width: '100%',
            flexGrow: 1,
            overflowY: 'hidden',
            alignItems: 'stretch',
          }}
        >
          <SimpleFlexColumn
            style={{
              height: '100%',
              width: 44,
              backgroundColor: UtopiaTheme.color.leftMenuBackground.value,
            }}
          >
            <Menubar />
          </SimpleFlexColumn>
          {leftMenuExpanded ? (
            <ResizableFlexColumn
              className='LeftPaneShell'
              style={{
                height: '100% !important',
                overflowX: 'scroll',
                backgroundColor: UtopiaTheme.color.leftPaneBackground.value,
              }}
              size={{ width: leftMenuWidth, height: '100%' }}
              minWidth={5}
              onResizeStop={onResizeStop}
            >
              <LeftPaneComponent />
            </ResizableFlexColumn>
          ) : null}
          <SimpleFlexRow
            className='editor-shell'
            style={{
              flexGrow: 1,
              alignItems: 'stretch',
              borderRight: `1px solid ${UtopiaTheme.color.neutralBorder.value}`,
            }}
          >
            <SimpleFlexColumn
              className='EditorSpace'
              style={{
                flexGrow: 1,
                backgroundColor: UtopiaTheme.color.slightlyEmphasizedBackground.value,
              }}
            >
              <SimpleFlexRow
                className='tabRail'
                style={{
                  minHeight: 30,
                  height: 30,
                  borderBottom: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
                  alignItems: 'stretch',
                  justifyContent: 'stretch',
                  backgroundColor: 'transparent',
                  overflowX: 'hidden',
                }}
              >
                <SimpleFlexRow style={{ flexGrow: 1, alignItems: 'stretch' }}>
                  <FileTabs />
                </SimpleFlexRow>
                <FlexRow
                  css={{
                    marginLeft: 8,
                    marginRight: 8,
                    '& > :not(:first-of-type):not(:last-of-type)': {
                      marginRight: 8,
                    },
                    '& > :first-of-type': {
                      marginRight: 8,
                    },
                    '& > :last-of-type': {
                      marginRight: 0,
                    },
                  }}
                ></FlexRow>
              </SimpleFlexRow>

              <SimpleFlexRow
                className='openTabShell'
                style={{
                  flexGrow: 1,
                  alignItems: 'stretch',
                  justifyContent: 'stretch',
                  overflowX: 'hidden',
                }}
              >
                <OpenFileEditor
                  runtimeErrors={runtimeErrors}
                  onRuntimeError={onRuntimeError}
                  clearRuntimeErrors={clearRuntimeErrors}
                  canvasConsoleLogs={canvasConsoleLogsState}
                  clearConsoleLogs={clearConsoleLogs}
                  addToConsoleLogs={addToConsoleLogs}
                />
              </SimpleFlexRow>
            </SimpleFlexColumn>
            {/* insert more columns here */}
            {previewVisible ? (
              <ResizableFlexColumn
                style={{ borderLeft: `1px solid ${colorTheme.secondaryBorder.value}` }}
                enable={{
                  left: true,
                  right: false,
                }}
                defaultSize={{
                  width: 350,
                  height: '100%',
                }}
              >
                <SimpleFlexRow
                  id='PreviewTabRail'
                  style={{
                    height: UtopiaTheme.layout.rowHeight.smaller,
                    borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
                    alignItems: 'stretch',
                  }}
                >
                  <TabComponent
                    label='Preview'
                    selected
                    icon={<LargerIcons.PreviewPane color='blue' />}
                    onClose={onClosePreview}
                  />
                </SimpleFlexRow>
                <PreviewColumn />
              </ResizableFlexColumn>
            ) : null}
          </SimpleFlexRow>
        </SimpleFlexRow>
      </SimpleFlexColumn>
      <ModalComponent />
      <ToastRenderer />
      <CanvasCursorComponent />
      <HelpTriangle />
    </SimpleFlexRow>
  )
})

const ModalComponent = betterReactMemo('ModalComponent', (): React.ReactElement<any> | null => {
  const { modal, dispatch } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      modal: store.editor.modal,
    }
  })
  if (modal != null) {
    if (modal.type === 'file-delete') {
      return <ConfirmDeleteDialog dispatch={dispatch} filePath={modal.filePath} />
    } else if (modal.type === 'file-close') {
      return <ConfirmCloseDialog dispatch={dispatch} editorTab={modal.editorTab} />
    }
  }
  return null
})

export function EditorComponent() {
  return (
    <DndProvider backend={Backend}>
      <EditorComponentInner />
    </DndProvider>
  )
}

const HelpTriangle = () => (
  <div
    style={{
      position: 'absolute',
      bottom: '16px',
      right: '16px',
      width: '50px',
      height: '39px',
    }}
  >
    <a href='http://spectrum.chat/utopia' target='_blank' rel='noopener noreferrer'>
      <img
        src='/static/brand/triangle-question-brandpurple-50x39@2x.png'
        alt='help'
        style={{ width: '50px', height: '39px' }}
      />
    </a>
  </div>
)

interface OpenFileEditorProps {
  runtimeErrors: Array<RuntimeErrorInfo>
  onRuntimeError: (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => void
  clearRuntimeErrors: () => void
  canvasConsoleLogs: Array<ConsoleLog>
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
}

const OpenFileEditor = betterReactMemo('OpenFileEditor', (props: OpenFileEditorProps) => {
  const { noFileOpen, isUiJsFileOpen, areReleaseNotesOpen } = useEditorState((store) => {
    const selectedFile = getOpenFile(store.editor)
    const openEditorTab = getOpenEditorTab(store.editor)
    return {
      noFileOpen: openEditorTab == null,
      isUiJsFileOpen: selectedFile != null && isUIJSFile(selectedFile),
      areReleaseNotesOpen: openEditorTab != null && isReleaseNotesTab(openEditorTab),
    }
  })

  if (noFileOpen) {
    return <Subdued>No file open</Subdued>
  } else if (areReleaseNotesOpen) {
    return <ReleaseNotesContent />
  } else if (isUiJsFileOpen) {
    return <SplitViewCanvasRoot {...props} />
  } else {
    return (
      <ScriptEditor
        relevantPanel={'misccodeeditor'}
        runtimeErrors={props.runtimeErrors}
        canvasConsoleLogs={props.canvasConsoleLogs}
      />
    )
  }
})
OpenFileEditor.displayName = 'OpenFileEditor'

const CanvasCursorComponent = betterReactMemo('CanvasCursorComponent', () => {
  const cursor = useEditorState((store) => {
    return Utils.defaultIfNull(store.editor.canvas.cursor, getCursorFromDragState(store.editor))
  })
  return cursor == null ? null : (
    <div
      key='cursor-area'
      style={{
        position: 'fixed',
        left: 0,
        top: 0,
        width: '100vw',
        height: '100vh',
        cursor: cursor,
      }}
    />
  )
})

const ToastRenderer = betterReactMemo('ToastRenderer', () => {
  const toasts = useEditorState((store) => store.editor.toasts)

  return (
    <FlexColumn
      key={'toast-stack'}
      style={{
        position: 'fixed',
        bottom: 40,
        justifyContent: 'center',
        left: '30%',
        overflow: 'scroll',
        maxHeight: '50%',
      }}
    >
      {toasts.map((toast, index) => (
        <Toast
          key={`toast-${index}`}
          message={toast.message}
          level={toast.level}
          persistent={toast.persistent}
        />
      ))}
    </FlexColumn>
  )
})
