/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import { chrome as isChrome } from 'platform-detect'
import React from 'react'
import { DndProvider } from 'react-dnd'
import { HTML5Backend } from 'react-dnd-html5-backend'
import { IS_BROWSER_TEST_DEBUG, IS_TEST_ENVIRONMENT } from '../../common/env-vars'
import { projectURLForProject } from '../../core/shared/utils'
import { isFeatureEnabled } from '../../utils/feature-switches'
import Keyboard from '../../utils/keyboard'
import { Modifier } from '../../utils/modifiers'
import {
  FlexColumn,
  LargerIcons,
  ResizableFlexColumn,
  SimpleFlexColumn,
  SimpleFlexRow,
  ColorThemeStyleComponent,
  TabComponent,
  useColorTheme,
  UtopiaTheme,
} from '../../uuiui'
import CanvasActions from '../canvas/canvas-actions'
import {
  createInteractionViaKeyboard,
  updateInteractionViaKeyboard,
} from '../canvas/canvas-strategies/interaction-state'
import { useClearKeyboardInteraction } from '../canvas/controls/select-mode/select-mode-hooks'
import { DesignPanelRoot } from '../canvas/design-panel-root'
import { Toast } from '../common/notices'
import { ConfirmDeleteDialog } from '../filebrowser/confirm-delete-dialog'
import { ConfirmOverwriteDialog } from '../filebrowser/confirm-overwrite-dialog'
import { ConfirmRevertDialogProps } from '../filebrowser/confirm-revert-dialog'
import { ConfirmRevertAllDialogProps } from '../filebrowser/confirm-revert-all-dialog'
import { LeftPaneComponent } from '../navigator/left-pane'
import { PreviewColumn } from '../preview/preview-pane'
import TitleBar from '../titlebar/title-bar'
import * as EditorActions from './actions/action-creators'
import { FatalIndexedDBErrorComponent } from './fatal-indexeddb-error-component'
import { editorIsTarget, handleKeyDown, handleKeyUp } from './global-shortcuts'
import { BrowserInfoBar, LoginStatusBar } from './notification-bar'
import { applyShortcutConfigurationToDefaults } from './shortcut-definitions'
import {
  emptyGithubSettings,
  githubOperationLocksEditor,
  LeftMenuTab,
  LeftPaneDefaultWidth,
} from './store/editor-state'
import { useEditorState, useRefEditorState } from './store/store-hook'
import { refreshGithubData } from '../../core/shared/github'
import { ConfirmDisconnectBranchDialog } from '../filebrowser/confirm-branch-disconnect'
import { when } from '../../utils/react-conditionals'
import { LowPriorityStoreProvider } from './store/low-priority-store'

function pushProjectURLToBrowserHistory(projectId: string, projectName: string): void {
  // Make sure we don't replace the query params
  const queryParams = window.top?.location.search
  const projectURL = projectURLForProject(projectId, projectName)
  const title = `Utopia ${projectName}`
  window.top?.history.pushState({}, title, `${projectURL}${queryParams}`)
}

const GITHUB_REFRESH_INTERVAL_MILLISECONDS = 30_000

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
  useGithubData()

  const editorStoreRef = useRefEditorState((store) => store)
  const colorTheme = useColorTheme()
  const onWindowMouseUp = React.useCallback(
    (event: MouseEvent) => {
      editorStoreRef.current.dispatch(
        [EditorActions.updateMouseButtonsPressed(null, event.button)],
        'everyone',
      )
    },
    [editorStoreRef],
  )
  const onWindowMouseDown = React.useCallback(
    (event: MouseEvent) => {
      editorStoreRef.current.dispatch(
        [EditorActions.updateMouseButtonsPressed(event.button, null)],
        'everyone',
      )
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

  const setClearKeyboardInteraction = useClearKeyboardInteraction(editorStoreRef)

  const onWindowKeyDown = React.useCallback(
    (event: KeyboardEvent) => {
      if (
        isFeatureEnabled('Canvas Strategies') &&
        editorIsTarget(event, editorStoreRef.current.editor)
      ) {
        const key = Keyboard.keyCharacterForCode(event.keyCode)
        const modifiers = Modifier.modifiersForKeyboardEvent(event)

        // TODO: maybe we should not whitelist keys, just check if Keyboard.keyIsModifer(key) is false
        const existingInteractionSession = editorStoreRef.current.editor.canvas.interactionSession
        if (
          (Keyboard.keyIsModifier(key) || key === 'space') &&
          existingInteractionSession != null
        ) {
          editorStoreRef.current.dispatch(
            [
              CanvasActions.createInteractionSession(
                updateInteractionViaKeyboard(existingInteractionSession, [key], [], modifiers, {
                  type: 'KEYBOARD_CATCHER_CONTROL',
                }),
              ),
            ],
            'everyone',
          )
        } else if (Keyboard.keyIsInteraction(key)) {
          const action =
            existingInteractionSession == null
              ? CanvasActions.createInteractionSession(
                  createInteractionViaKeyboard([key], modifiers, {
                    type: 'KEYBOARD_CATCHER_CONTROL',
                  }),
                )
              : CanvasActions.createInteractionSession(
                  updateInteractionViaKeyboard(existingInteractionSession, [key], [], modifiers, {
                    type: 'KEYBOARD_CATCHER_CONTROL',
                  }),
                )

          editorStoreRef.current.dispatch([action], 'everyone')

          setClearKeyboardInteraction()
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
    [editorStoreRef, namesByKey, setClearKeyboardInteraction],
  )

  const onWindowKeyUp = React.useCallback(
    (event: KeyboardEvent) => {
      if (isFeatureEnabled('Canvas Strategies')) {
        const existingInteractionSession = editorStoreRef.current.editor.canvas.interactionSession
        if (existingInteractionSession != null) {
          if (isFeatureEnabled('Keyboard up clears interaction')) {
            const action = CanvasActions.clearInteractionSession(true)
            editorStoreRef.current.dispatch([action], 'everyone')
          } else {
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
    window.addEventListener('mouseup', onWindowMouseUp, true)
    window.addEventListener('keydown', onWindowKeyDown)
    window.addEventListener('keyup', onWindowKeyUp)
    window.addEventListener('contextmenu', preventDefault)
    return function cleanup() {
      window.removeEventListener('mousedown', onWindowMouseDown, true)
      window.removeEventListener('mouseup', onWindowMouseUp, true)
      window.removeEventListener('keydown', onWindowKeyDown)
      window.removeEventListener('keyup', onWindowKeyUp)
      window.removeEventListener('contextmenu', preventDefault)
    }
  }, [onWindowMouseDown, onWindowMouseUp, onWindowKeyDown, onWindowKeyUp, preventDefault])

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
    if (IS_BROWSER_TEST_DEBUG) {
      return
    }
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
          EditorActions.setLeftMenuTab(LeftMenuTab.Project),
        ]
        dispatch(actions, 'everyone')
      }
    },
    [dispatch],
  )

  return (
    <>
      <ColorThemeStyleComponent />
      <SimpleFlexRow
        className='editor-main-vertical-and-modals'
        style={{
          height: '100%',
          width: '100%',
          overscrollBehaviorX: 'contain',
          color: colorTheme.fg1.value,
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
          <LowPriorityStoreProvider>
            {(isChrome as boolean) ? null : <BrowserInfoBar />}
            <LoginStatusBar />
            <TitleBar />
          </LowPriorityStoreProvider>

          <SimpleFlexRow
            className='editor-main-horizontal'
            style={{
              width: '100%',
              flexGrow: 1,
              overflowY: 'hidden',
              alignItems: 'stretch',
            }}
          >
            <div
              className='LeftPaneShell'
              style={{
                height: '100%',
                display: 'flex',
                flexShrink: 0,
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
        <LockedOverlay />
      </SimpleFlexRow>
    </>
  )
})

const useGithubData = (): void => {
  const dispatch = useEditorState((store) => store.dispatch, 'Dispatch')
  const {
    githubAuthenticated,
    githubRepo,
    githubOperations,
    branchName,
    githubChecksums,
    githubUserDetails,
  } = useEditorState(
    (store) => ({
      githubAuthenticated: store.userState.githubState.authenticated,
      githubRepo: store.editor.githubSettings.targetRepository,
      githubOperations: store.editor.githubOperations,
      branchName: store.editor.githubSettings.branchName,
      githubChecksums: store.editor.githubChecksums,
      githubUserDetails: store.editor.githubData.githubUserDetails,
    }),
    'Github data',
  )

  const refresh = React.useCallback(() => {
    void refreshGithubData(
      dispatch,
      githubAuthenticated,
      githubRepo,
      branchName,
      githubChecksums,
      githubUserDetails,
    )
  }, [dispatch, githubAuthenticated, githubRepo, branchName, githubChecksums, githubUserDetails])

  // perform a straight refresh then the repo or the auth change
  React.useEffect(() => refresh(), [refresh])

  // schedule a repeat refresh every GITHUB_REFRESH_INTERVAL
  React.useEffect(() => {
    if (githubOperations.length > 0) {
      // ignore scheduling if there are already operations going on
      return
    }

    let interval = setInterval(refresh, GITHUB_REFRESH_INTERVAL_MILLISECONDS)
    return function () {
      clearInterval(interval)
    }
  }, [refresh, githubOperations])
}

const ModalComponent = React.memo((): React.ReactElement<any> | null => {
  const { modal, dispatch, currentBranch } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      modal: store.editor.modal,
      currentBranch: store.editor.githubSettings.branchName,
    }
  }, 'ModalComponent')
  if (modal != null) {
    switch (modal.type) {
      case 'file-delete':
        return <ConfirmDeleteDialog dispatch={dispatch} filePath={modal.filePath} />
      case 'file-overwrite':
        return <ConfirmOverwriteDialog dispatch={dispatch} files={modal.files} />
      case 'file-revert':
        return (
          <ConfirmRevertDialogProps
            dispatch={dispatch}
            status={modal.status}
            filePath={modal.filePath}
          />
        )
      case 'file-revert-all':
        return <ConfirmRevertAllDialogProps dispatch={dispatch} />
      case 'disconnect-github-project':
        if (currentBranch != null) {
          return <ConfirmDisconnectBranchDialog dispatch={dispatch} branchName={currentBranch} />
        }
        break
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
    <DndProvider backend={HTML5Backend} context={window}>
      <EditorComponentInner {...props} />
    </DndProvider>
  )
}

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

function handleEventNoop(e: React.MouseEvent | React.KeyboardEvent) {
  e.stopPropagation()
  e.preventDefault()
}

const LockedOverlay = React.memo(() => {
  const leftMenuExpanded = useEditorState(
    (store) => store.editor.leftMenu.expanded,
    'EditorComponentInner leftMenuExpanded',
  )

  const editorLocked = useEditorState(
    (store) => store.editor.githubOperations.some((op) => githubOperationLocksEditor(op)),
    'EditorComponentInner editorLocked',
  )

  const refreshingDependencies = useEditorState(
    (store) => store.editor.refreshingDependencies,
    'EditorComponentInner refreshingDependencies',
  )

  const anim = keyframes`
    from {
      opacity: 0;
    }
    to {
      opacity: 0.2;
    }
  `

  const locked = React.useMemo(() => {
    return editorLocked || refreshingDependencies
  }, [editorLocked, refreshingDependencies])

  if (!locked) {
    return null
  }

  return (
    <div
      onMouseDown={handleEventNoop}
      onMouseUp={handleEventNoop}
      onClick={handleEventNoop}
      onKeyDown={handleEventNoop}
      onKeyUp={handleEventNoop}
      style={{
        position: 'fixed',
        top: 0,
        left: leftMenuExpanded ? LeftPaneDefaultWidth : 0,
        width: '100vw',
        height: '100vh',
        backgroundColor: '#00000044',
        zIndex: 30,
        transition: 'all .1s ease-in-out',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
      }}
      css={css`
        animation: ${anim} 0.3s ease-in-out;
      `}
    >
      {when(
        refreshingDependencies,
        <div
          style={{
            opacity: 1,
            fontSize: 12,
            fontWeight: 500,
            background: '#fff',
            padding: 30,
            borderRadius: 2,
          }}
        >
          Updating dependencies…
        </div>,
      )}
    </div>
  )
})
