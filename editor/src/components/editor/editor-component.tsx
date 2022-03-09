/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import { ResizeDirection } from 're-resizable'
import React from 'react'
import { DndProvider } from 'react-dnd'
import Backend from 'react-dnd-html5-backend'
import Utils from '../../utils/utils'
import { FancyError, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { getCursorFromDragState } from '../canvas/canvas-utils'
import { DesignPanelRoot } from '../canvas/design-panel-root'
import { resizeLeftPane } from '../common/actions'
import { ConfirmCloseDialog } from '../filebrowser/confirm-close-dialog'
import { ConfirmDeleteDialog } from '../filebrowser/confirm-delete-dialog'
import { Menubar } from '../menubar/menubar'
import { LeftPaneComponent } from '../navigator/left-pane'
import { PreviewColumn } from '../preview/preview-pane'
import { ReleaseNotesContent } from '../documentation/release-notes'
import { EditorDispatch, LoginState } from './action-types'
import * as EditorActions from './actions/action-creators'
import { handleKeyDown, handleKeyUp } from './global-shortcuts'
import { StateHistory } from './history'
import { LoginStatusBar, BrowserInfoBar } from './notification-bar'
import {
  ConsoleLog,
  getOpenFile,
  getOpenTextFileKey,
  LeftMenuTab,
  LeftPaneDefaultWidth,
  StoryboardFilePath,
} from './store/editor-state'
import { useEditorState, useRefEditorState } from './store/store-hook'
import { isParsedTextFile } from '../../core/shared/project-file-types'
import { isLiveMode, dragAndDropInsertionSubject, EditorModes, isSelectMode } from './editor-modes'
import { Toast } from '../common/notices'
import { chrome as isChrome } from 'platform-detect'
import { applyShortcutConfigurationToDefaults } from './shortcut-definitions'
import { PROPERTY_CONTROLS_INFO_BASE_URL } from '../../common/env-vars'
import {
  SimpleFlexRow,
  SimpleFlexColumn,
  UtopiaTheme,
  FlexRow,
  ResizableFlexColumn,
  useColorTheme,
  TabComponent,
  LargerIcons,
  Subdued,
  FlexColumn,
} from '../../uuiui'
import { createIframeUrl, projectURLForProject } from '../../core/shared/utils'
import { setBranchNameFromURL } from '../../utils/branches'
import { FatalIndexedDBErrorComponent } from './fatal-indexeddb-error-component'
import { isFeatureEnabled } from '../../utils/feature-switches'
import Keyboard from '../../utils/keyboard'
import { Modifier } from '../../utils/modifiers'
import CanvasActions from '../canvas/canvas-actions'
import { updateInteractionViaKeyboard } from '../canvas/canvas-strategies/interaction-state'

function pushProjectURLToBrowserHistory(projectId: string, projectName: string): void {
  // Make sure we don't replace the query params
  const queryParams = window.top.location.search
  const projectURL = projectURLForProject(projectId, projectName)
  const title = `Utopia ${projectName}`
  window.top.history.pushState({}, title, `${projectURL}${queryParams}`)
}

interface NumberSize {
  width: number
  height: number
}

export interface EditorProps {}

function useDelayedValueHook(inputValue: boolean, delayMs: number): boolean {
  const [returnValue, setReturnValue] = React.useState(inputValue)
  React.useEffect(() => {
    let timerID: any = undefined
    if (inputValue) {
      // we do not delay the toggling if the input value is true
      setReturnValue(true)
    } else {
      timerID = setTimeout(() => {
        setReturnValue(false)
      }, delayMs)
    }
    return function cleanup() {
      clearTimeout(timerID)
    }
  }, [inputValue, delayMs])
  return returnValue
}

export const EditorComponentInner = React.memo((props: EditorProps) => {
  const editorStoreRef = useRefEditorState((store) => store)
  const colorTheme = useColorTheme()
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
        activeElement.getAttribute('data-inspector-input') != null &&
        (activeElement as any).blur != null
      ) {
        // OMG what a nightmare! This is the only way of keeping the Inspector fast and ensuring the blur handler for inputs
        // is called before triggering a change that might change the selection
        ;(activeElement as any).blur()
      }
    },
    [editorStoreRef],
  )

  const namesByKey = React.useMemo(() => {
    return applyShortcutConfigurationToDefaults(editorStoreRef.current.userState.shortcutConfig)
  }, [editorStoreRef])

  const onWindowKeyDown = React.useCallback(
    (event: KeyboardEvent) => {
      if (isFeatureEnabled('Canvas Strategies')) {
        const existingInteractionSession = editorStoreRef.current.editor.canvas.interactionSession
        // TODO: we don't handle new interaction started with keyboard now
        if (existingInteractionSession != null) {
          const action = CanvasActions.createInteractionSession(
            updateInteractionViaKeyboard(
              existingInteractionSession,
              [Keyboard.keyCharacterForCode(event.keyCode)],
              [],
              Modifier.modifiersForKeyboardEvent(event),
              { type: 'KEYBOARD_CATCHER_CONTROL' },
            ),
          )
          editorStoreRef.current.dispatch([action], 'everyone')
        }
      }

      handleKeyDown(
        event,
        editorStoreRef.current.editor,
        editorStoreRef.current.derived,
        namesByKey,
        editorStoreRef.current.dispatch,
      )
    },
    [editorStoreRef, namesByKey],
  )

  const onWindowKeyUp = React.useCallback(
    (event) => {
      if (isFeatureEnabled('Canvas Strategies')) {
        const existingInteractionSession = editorStoreRef.current.editor.canvas.interactionSession
        if (existingInteractionSession != null) {
          const action = CanvasActions.createInteractionSession(
            updateInteractionViaKeyboard(
              existingInteractionSession,
              [],
              [Keyboard.keyCharacterForCode(event.keyCode)],
              Modifier.modifiersForKeyboardEvent(event),
              { type: 'KEYBOARD_CATCHER_CONTROL' },
            ),
          )
          editorStoreRef.current.dispatch([action], 'everyone')
        }
      }

      handleKeyUp(event, editorStoreRef.current.editor, namesByKey, editorStoreRef.current.dispatch)
    },
    [editorStoreRef, namesByKey],
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
      window.removeEventListener('mousedown', onWindowMouseDown, true)
      window.removeEventListener('keydown', onWindowKeyDown)
      window.removeEventListener('keyup', onWindowKeyUp)
      window.removeEventListener('contextmenu', preventDefault)
    }
  }, [onWindowMouseDown, onWindowKeyDown, onWindowKeyUp, preventDefault])

  const dispatch = useEditorState((store) => store.dispatch, 'EditorComponentInner dispatch')
  const projectName = useEditorState(
    (store) => store.editor.projectName,
    'EditorComponentInner projectName',
  )
  const projectId = useEditorState((store) => store.editor.id, 'EditorComponentInner projectId')
  const previewVisible = useEditorState(
    (store) => store.editor.preview.visible,
    'EditorComponentInner previewVisible',
  )
  const leftMenuExpanded = useEditorState(
    (store) => store.editor.leftMenu.expanded,
    'EditorComponentInner leftMenuExpanded',
  )

  const delayedLeftMenuExpanded = useDelayedValueHook(leftMenuExpanded, 200)

  React.useEffect(() => {
    document.title = projectName + ' - Utopia'
  }, [projectName])

  React.useEffect(() => {
    if (projectId) {
      pushProjectURLToBrowserHistory(projectId, projectName)
    }
  }, [projectName, projectId])

  const onClosePreview = React.useCallback(
    () => dispatch([EditorActions.setPanelVisibility('preview', false)]),
    [dispatch],
  )

  const startDragInsertion = React.useCallback(
    (event: React.DragEvent<HTMLDivElement>) => {
      const draggedTypes = event.nativeEvent?.dataTransfer?.types
      const isDraggedFile =
        draggedTypes != null && draggedTypes.length === 1 && draggedTypes[0] === 'Files'
      if (isDraggedFile) {
        const actions = [
          EditorActions.setPanelVisibility('leftmenu', true),
          EditorActions.setLeftMenuTab(LeftMenuTab.Contents),
        ]
        dispatch(actions, 'everyone')
      }
    },
    [dispatch],
  )

  const vscodeBridgeReady = useEditorState((store) => {
    return store.editor.vscodeBridgeReady
  }, 'EditorComponentInner vscodeBridgeReady')

  return (
    <>
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
          {(isChrome as boolean) ? null : <BrowserInfoBar />}
          <LoginStatusBar />

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
                backgroundColor: colorTheme.leftMenuBackground.value,
              }}
            >
              <Menubar />
            </SimpleFlexColumn>
            <div
              className='LeftPaneShell'
              style={{
                height: '100%',
                flexShrink: 0,
                transition: 'all .1s ease-in-out',
                width: leftMenuExpanded ? LeftPaneDefaultWidth : 0,
                overflowX: 'scroll',
                backgroundColor: colorTheme.leftPaneBackground.value,
              }}
            >
              {delayedLeftMenuExpanded ? <LeftPaneComponent /> : null}
            </div>
            <SimpleFlexRow
              className='editor-shell'
              style={{
                flexGrow: 1,
                alignItems: 'stretch',
                borderRight: `1px solid ${colorTheme.neutralBorder.value}`,
                backgroundColor: colorTheme.neutralBackground.value,
              }}
            >
              <SimpleFlexRow
                className='openTabShell'
                style={{
                  flexGrow: 1,
                  alignItems: 'stretch',
                  justifyContent: 'stretch',
                  overflowX: 'hidden',
                }}
              >
                <DesignPanelRoot />
              </SimpleFlexRow>
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
                      icon={<LargerIcons.PreviewPane color='primary' />}
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
        <EditorCursorComponent />
      </SimpleFlexRow>
    </>
  )
})

const ModalComponent = React.memo((): React.ReactElement<any> | null => {
  const { modal, dispatch } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      modal: store.editor.modal,
    }
  }, 'ModalComponent')
  if (modal != null) {
    if (modal.type === 'file-delete') {
      return <ConfirmDeleteDialog dispatch={dispatch} filePath={modal.filePath} />
    }
  }
  return null
})

export function EditorComponent(props: EditorProps) {
  const indexedDBFailed = useEditorState(
    (store) => store.editor.indexedDBFailed,
    'EditorComponent indexedDBFailed',
  )

  return indexedDBFailed ? (
    <FatalIndexedDBErrorComponent />
  ) : (
    <DndProvider backend={Backend}>
      <EditorComponentInner {...props} />
    </DndProvider>
  )
}

const EditorCursorComponent = React.memo(() => {
  const cursor = useEditorState((store) => {
    return Utils.defaultIfNull(store.editor.canvas.cursor, getCursorFromDragState(store.editor))
  }, 'EditorCursorComponent cursor')

  const styleProps = React.useMemo(() => {
    let workingStyleProps: React.CSSProperties = {
      position: 'fixed',
      left: 0,
      top: 0,
      width: '100vw',
      height: '100vh',
      pointerEvents: 'none',
      zIndex: 9999999,
    }
    if (cursor != null) {
      workingStyleProps.cursor = cursor
      workingStyleProps.pointerEvents = 'all'
    }
    return workingStyleProps
  }, [cursor])

  return <div key='cursor-area' id='cursor-overlay' style={styleProps} />
})

const ToastRenderer = React.memo(() => {
  const toasts = useEditorState((store) => store.editor.toasts, 'ToastRenderer')

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
        zIndex: 100,
      }}
    >
      {toasts.map((toast, index) => (
        <Toast
          key={`toast-${index}`}
          message={toast.message}
          level={toast.level}
          persistent={toast.persistent}
          id={toast.id}
        />
      ))}
    </FlexColumn>
  )
})
