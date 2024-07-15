/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import { chrome as isChrome } from 'platform-detect'
import React from 'react'
import { DndProvider } from 'react-dnd'
import { HTML5Backend } from 'react-dnd-html5-backend'
import { IS_TEST_ENVIRONMENT } from '../../common/env-vars'
import { assertNever, projectURLForProject } from '../../core/shared/utils'
import Keyboard from '../../utils/keyboard'
import { Modifier } from '../../utils/modifiers'
import {
  FlexColumn,
  LargerIcons,
  ResizableFlexColumn,
  SimpleFlexColumn,
  SimpleFlexRow,
  ColorThemeComponent,
  TabComponent,
  useColorTheme,
  UtopiaTheme,
  UtopiaStyles,
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
import { ConfirmRevertDialog } from '../filebrowser/confirm-revert-dialog'
import { ConfirmRevertAllDialog } from '../filebrowser/confirm-revert-all-dialog'
import { PreviewColumn } from '../preview/preview-pane'
import * as EditorActions from './actions/action-creators'
import { FatalIndexedDBErrorComponent } from './fatal-indexeddb-error-component'
import { editorIsTarget, handleKeyDown, handleKeyUp } from './global-shortcuts'
import { BrowserInfoBar, LoginStatusBar } from './notification-bar'
import { applyShortcutConfigurationToDefaults } from './shortcut-definitions'
import type { GithubOperation } from './store/editor-state'
import { githubOperationLocksEditor, LeftMenuTab, RightMenuTab } from './store/editor-state'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from './store/store-hook'
import { ConfirmDisconnectBranchDialog } from '../filebrowser/confirm-branch-disconnect'
import { when } from '../../utils/react-conditionals'
import { LowPriorityStoreProvider } from './store/store-context-providers'
import { useDispatch } from './store/dispatch-context'
import type { EditorAction } from './action-types'
import { EditorCommon } from './editor-component-common'
import { notice } from '../common/notice'
import { ProjectServerStateUpdater } from './store/project-server-state'
import { RoomProvider, initialPresence, useRoom, initialStorage } from '../../../liveblocks.config'
import { generateUUID } from '../../utils/utils'
import { isLiveblocksEnabled } from './liveblocks-utils'
import type { Storage, Presence, RoomEvent, UserMeta } from '../../../liveblocks.config'
import LiveblocksProvider from '@liveblocks/yjs'
import { EditorModes } from './editor-modes'
import { useDataThemeAttributeOnBody } from '../../core/commenting/comment-hooks'
import { CollaborationStateUpdater } from './store/collaboration-state'
import { GithubRepositoryCloneFlow } from '../github/github-repository-clone-flow'
import { getPermissions } from './store/permissions'
import { CommentMaintainer } from '../../core/commenting/comment-maintainer'
import { useIsLoggedIn, useLiveblocksConnectionListener } from '../../core/shared/multiplayer-hooks'
import { ForkSearchParamKey, ProjectForkFlow } from './project-fork-flow'
import { isRoomId, projectIdToRoomId } from '../../utils/room-id'
import { SharingDialog } from './sharing-dialog'
import {
  AccessLevelParamKey,
  CloneParamKey,
  GithubBranchParamKey,
} from './persistence/persistence-backend'
import {
  RemixNavigationAtom,
  useUpdateActiveRemixSceneOnSelectionChange,
} from '../canvas/remix/utopia-remix-root-component'
import { useDefaultCollapsedViews } from './use-default-collapsed-views'
import { useCreateCallbackToShowComponentPicker } from '../navigator/navigator-item/component-picker-context-menu'
import { useGithubPolling } from '../../core/shared/github/helpers'
import { useAtom } from 'jotai'
import { clearOpenMenuIds } from '../../core/shared/menu-state'

const liveModeToastId = 'play-mode-toast'

function pushProjectURLToBrowserHistory(
  projectId: string,
  projectName: string,
  forking: boolean,
): void {
  // Make sure we don't replace the query params
  const queryParams = new URLSearchParams(window.top?.location.search)
  if (forking) {
    // …but if it's forking, remove the fork param
    queryParams.delete(ForkSearchParamKey)
  }
  // remove one-time creation params
  queryParams.delete(AccessLevelParamKey)
  queryParams.delete(CloneParamKey)
  queryParams.delete(GithubBranchParamKey)

  const queryParamsStr = queryParams.size > 0 ? `?${queryParams.toString()}` : ''

  const projectURL = projectURLForProject(projectId, projectName)
  const title = `Utopia ${projectName}`

  window.top?.history.replaceState({}, title, `${projectURL}${queryParamsStr}`)
}

function githubOperationPrettyNameForOverlay(op: GithubOperation): string {
  switch (op.name) {
    case 'commitAndPush':
      return 'Saving to GitHub'
    case 'listBranches':
      return 'Listing branches from GitHub'
    case 'loadBranch':
      return 'Loading branch from GitHub'
    case 'loadRepositories':
      return 'Loading Repositories'
    case 'updateAgainstBranch':
      return 'Updating against branch from GitHub'
    case 'listPullRequestsForBranch':
      return 'Listing GitHub pull requests'
    case 'saveAsset':
      return 'Saving asset to GitHub'
    case 'searchRepository':
      return 'Searching public repository'
    default:
      assertNever(op)
  }
}

export interface EditorProps {}

export const EditorComponentInner = React.memo((props: EditorProps) => {
  const room = useRoom()
  const dispatch = useDispatch()
  const editorStoreRef = useRefEditorState((store) => store)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const navigatorTargetsRef = useRefEditorState((store) => store.derived.navigatorTargets)
  const colorTheme = useColorTheme()
  const onWindowMouseUp = React.useCallback((event: MouseEvent) => {
    return [EditorActions.updateMouseButtonsPressed(null, event.button)]
  }, [])
  const onWindowMouseDown = React.useCallback(
    (event: MouseEvent) => {
      let actions: Array<EditorAction> = []
      actions.push(EditorActions.updateMouseButtonsPressed(event.button, null))
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
          actions.push(EditorActions.closePopup())
        }
      }
      return actions
    },
    [editorStoreRef],
  )

  const inputBlurForce = React.useCallback((event: MouseEvent) => {
    // Keep this outside of the common handling because it needs to be triggered with `capture` set to `true`,
    // so that it fires before the inspector disappears.
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
  }, [])

  const namesByKey = React.useMemo(() => {
    return applyShortcutConfigurationToDefaults(editorStoreRef.current.userState.shortcutConfig)
  }, [editorStoreRef])

  const setClearKeyboardInteraction = useClearKeyboardInteraction(editorStoreRef)

  const mode = useEditorState(Substores.restOfEditor, (store) => store.editor.mode, 'mode')
  React.useEffect(() => {
    setTimeout(() => {
      if (mode.type === 'live') {
        dispatch([
          EditorActions.showToast(
            notice(
              'You are in Live mode. Use ⌘ to select and scroll.',
              'NOTICE',
              true,
              liveModeToastId,
            ),
          ),
        ])
      } else {
        dispatch([EditorActions.removeToast(liveModeToastId)])
      }
    }, 0)
  }, [mode.type, dispatch])

  const onBeforeUnload = React.useCallback(
    (event: BeforeUnloadEvent) => {
      if (mode.type === 'live') {
        // Catch and check unintended navigation when the user is in live mode
        event.preventDefault()
        event.returnValue = ''
      }
    },
    [mode.type],
  )

  const showComponentPicker = useCreateCallbackToShowComponentPicker()

  React.useEffect(() => {
    clearOpenMenuIds()
  }, [])

  const onWindowKeyDown = React.useCallback(
    (event: KeyboardEvent) => {
      let actions: Array<EditorAction> = []
      if (editorIsTarget(event, editorStoreRef.current.editor)) {
        const key = Keyboard.keyCharacterForCode(event.keyCode)
        const modifiers = Modifier.modifiersForKeyboardEvent(event)

        // TODO: maybe we should not whitelist keys, just check if Keyboard.keyIsModifer(key) is false
        const existingInteractionSession = editorStoreRef.current.editor.canvas.interactionSession

        const cmdPressedThisFrame = event.key === 'Meta'

        if (
          existingInteractionSession != null &&
          existingInteractionSession.interactionData.type === 'KEYBOARD' &&
          cmdPressedThisFrame
        ) {
          // If cmd has been pressed this frame, we need to clear this session and start a new one
          actions.push(CanvasActions.clearInteractionSession(true))
        }

        if (
          (Keyboard.keyIsModifier(key) || key === 'space') &&
          existingInteractionSession != null &&
          !cmdPressedThisFrame
        ) {
          // Never update an existing interaction if cmd was just pressed
          actions.push(
            CanvasActions.createInteractionSession(
              updateInteractionViaKeyboard(existingInteractionSession, [key], [], modifiers, {
                type: 'KEYBOARD_CATCHER_CONTROL',
              }),
            ),
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

          actions.push(action)

          setClearKeyboardInteraction()
        }
      }

      actions.push(
        ...handleKeyDown(
          event,
          editorStoreRef.current.editor,
          editorStoreRef.current.userState.loginState,
          editorStoreRef.current.projectServerState,
          metadataRef,
          navigatorTargetsRef,
          namesByKey,
          dispatch,
          showComponentPicker,
        ),
      )
      return actions
    },
    [
      dispatch,
      editorStoreRef,
      metadataRef,
      navigatorTargetsRef,
      namesByKey,
      setClearKeyboardInteraction,
      showComponentPicker,
    ],
  )

  const onWindowKeyUp = React.useCallback(
    (event: KeyboardEvent) => {
      let actions: Array<EditorAction> = []
      const existingInteractionSession = editorStoreRef.current.editor.canvas.interactionSession
      if (existingInteractionSession != null) {
        if (
          existingInteractionSession.interactionData.type === 'KEYBOARD' &&
          event.key === 'Meta'
        ) {
          actions.push(CanvasActions.clearInteractionSession(true))
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
          actions.push(action)
        }
      }
      actions.push(...handleKeyUp(event, editorStoreRef.current.editor, namesByKey))
      return actions
    },
    [editorStoreRef, namesByKey],
  )

  const preventDefault = React.useCallback((event: MouseEvent) => {
    event.preventDefault()
  }, [])

  React.useEffect(() => {
    window.addEventListener('contextmenu', preventDefault)
    window.addEventListener('mousedown', inputBlurForce, true)
    window.addEventListener('beforeunload', onBeforeUnload)
    return function cleanup() {
      window.removeEventListener('contextmenu', preventDefault)
      window.removeEventListener('mousedown', inputBlurForce, true)
      window.addEventListener('beforeunload', onBeforeUnload)
    }
  }, [onBeforeUnload, preventDefault, inputBlurForce])

  const projectName = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.projectName,
    'EditorComponentInner projectName',
  )
  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'EditorComponentInner projectId',
  )
  const previewVisible = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.preview.visible,
    'EditorComponentInner previewVisible',
  )

  const yDoc = useEditorState(
    Substores.restOfStore,
    (store) => store.collaborativeEditingSupport.session?.mergeDoc,
    'EditorComponentInner yDoc',
  )

  React.useEffect(() => {
    if (yDoc != null && isRoomId(room.id)) {
      const yProvider = new LiveblocksProvider<Presence, Storage, UserMeta, RoomEvent>(room, yDoc)

      return () => {
        yProvider.destroy()
      }
    }

    return () => {}
  }, [yDoc, room])

  React.useEffect(() => {
    document.title = projectName + ' - Utopia'
  }, [projectName])

  const forking = useEditorState(Substores.restOfEditor, (store) => store.editor.forking, '')

  React.useEffect(() => {
    if (IS_TEST_ENVIRONMENT) {
      return
    }
    if (projectId != null) {
      pushProjectURLToBrowserHistory(projectId, projectName, forking)
      ;(window as any).utopiaProjectID = projectId
    }
  }, [projectName, projectId, forking])

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

  useSelectorWithCallback(
    Substores.userStateAndProjectServerState,
    (store) => ({ projectServerState: store.projectServerState, userState: store.userState }),
    (state) => {
      queueMicrotask(() => {
        let actions: EditorAction[] = []
        const permissions = getPermissions(state)
        if (!permissions.edit && permissions.comment) {
          actions.push(
            EditorActions.switchEditorMode(EditorModes.commentMode(null, 'not-dragging')),
            EditorActions.setRightMenuTab(RightMenuTab.Comments),
            EditorActions.setCodeEditorVisibility(false),
          )
        }
        dispatch(actions)
      })
    },
    'EditorComponentInner viewer mode',
  )

  useLiveblocksConnectionListener()

  useDefaultCollapsedViews()

  useGithubPolling()

  useClearSelectionOnNavigation()

  return (
    <>
      <ColorThemeComponent />
      <SimpleFlexRow
        className='editor-main-vertical-and-modals'
        style={{
          height: '100vh',
          width: '100vw',
          overscrollBehaviorX: 'contain',
          color: colorTheme.fg1.value,
          // the following line prevents user css overriding the editor font
          fontFamily: 'utopian-inter',
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
        <GithubRepositoryCloneFlow />
        <ProjectForkFlow />
        <LockedOverlay />
        <SharingDialog />
      </SimpleFlexRow>
      <EditorCommon
        mouseDown={onWindowMouseDown}
        mouseUp={onWindowMouseUp}
        keyDown={onWindowKeyDown}
        keyUp={onWindowKeyUp}
      />
      <CommentMaintainer />
    </>
  )
})

const ModalComponent = React.memo((): React.ReactElement<any> | null => {
  const dispatch = useDispatch()
  const currentBranch = useEditorState(
    Substores.github,
    (store) => {
      return store.editor.githubSettings.branchName
    },
    'ModalComponent branchName',
  )
  const modal = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.modal
    },
    'ModalComponent modal',
  )
  if (modal != null) {
    switch (modal.type) {
      case 'file-delete':
        return <ConfirmDeleteDialog dispatch={dispatch} filePath={modal.filePath} />
      case 'file-overwrite':
        return <ConfirmOverwriteDialog dispatch={dispatch} files={modal.files} />
      case 'file-revert':
        return (
          <ConfirmRevertDialog
            dispatch={dispatch}
            status={modal.status}
            filePath={modal.filePath}
          />
        )
      case 'file-revert-all':
        return <ConfirmRevertAllDialog dispatch={dispatch} />
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
    Substores.restOfEditor,
    (store) => store.editor.indexedDBFailed,
    'EditorComponent indexedDBFailed',
  )

  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'EditorComponent projectId',
  )

  const forkedFromProjectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.forkedFromProjectId,
    'EditorComponent forkedFromProjectId',
  )

  const loggedIn = useIsLoggedIn()

  const dispatch = useDispatch()

  useDataThemeAttributeOnBody()

  useUpdateActiveRemixSceneOnSelectionChange()

  const roomId = React.useMemo(
    () => (projectId == null ? generateUUID() : projectIdToRoomId(projectId)),
    [projectId],
  )
  return indexedDBFailed ? (
    <FatalIndexedDBErrorComponent />
  ) : (
    <RoomProvider
      id={roomId}
      autoConnect={isLiveblocksEnabled()}
      initialPresence={initialPresence()}
      initialStorage={initialStorage()}
    >
      <DndProvider backend={HTML5Backend} context={window}>
        <ProjectServerStateUpdater
          projectId={projectId}
          forkedFromProjectId={forkedFromProjectId}
          dispatch={dispatch}
          loggedIn={loggedIn}
        >
          <CollaborationStateUpdater projectId={projectId} dispatch={dispatch} loggedIn={loggedIn}>
            <EditorComponentInner {...props} />
          </CollaborationStateUpdater>
        </ProjectServerStateUpdater>
      </DndProvider>
    </RoomProvider>
  )
}

export const ToastRenderer = React.memo(() => {
  const toasts = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.toasts,
    'ToastRenderer',
  )

  return (
    <FlexColumn
      key={'toast-stack'}
      style={{
        zIndex: 100,
        gap: 10,
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
  const colorTheme = useColorTheme()

  const githubOperations = useEditorState(
    Substores.github,
    (store) => store.editor.githubOperations.filter((op) => githubOperationLocksEditor(op)),
    'LockedOverlay githubOperations',
  )

  const editorLocked = React.useMemo(() => githubOperations.length > 0, [githubOperations])

  const refreshingDependencies = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.refreshingDependencies,
    'LockedOverlay refreshingDependencies',
  )

  const forking = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.forking,
    'LockedOverlay forking',
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
    return editorLocked || refreshingDependencies || forking
  }, [editorLocked, refreshingDependencies, forking])

  const dialogContent = React.useMemo((): string | null => {
    if (refreshingDependencies) {
      return 'Refreshing dependencies…'
    }
    if (githubOperations.length > 0) {
      return `${githubOperationPrettyNameForOverlay(githubOperations[0])}…`
    }
    if (forking) {
      return 'Forking project…'
    }
    return null
  }, [refreshingDependencies, githubOperations, forking])

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
        left: 0,
        right: 0,
        bottom: 0,
        backgroundColor: '#00000033',
        zIndex: 30,
        transition: 'all .1s ease-in-out',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        cursor: 'wait',
      }}
      css={css`
        animation: ${anim} 0.3s ease-in-out;
      `}
    >
      {when(
        dialogContent != null,
        <div
          style={{
            opacity: 1,
            fontSize: 12,
            fontWeight: 500,
            backgroundColor: colorTheme.bg2.value,
            border: `1px solid ${colorTheme.neutralBorder.value}`,
            padding: 30,
            borderRadius: 2,
            boxShadow: UtopiaStyles.shadowStyles.high.boxShadow,
          }}
        >
          {dialogContent}
        </div>,
      )}
    </div>
  )
})

const useClearSelectionOnNavigation = () => {
  const dispatch = useDispatch()
  const [remixNavigation] = useAtom(RemixNavigationAtom)
  const paths = Object.values(remixNavigation)
    .map((n) => n?.location.pathname ?? '')
    .join('-')

  React.useEffect(() => {
    queueMicrotask(() => {
      dispatch([EditorActions.clearSelection()])
    })
  }, [dispatch, paths])
}
