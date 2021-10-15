// import feature switches so they are loaded before anything else can read them
import '../utils/feature-switches'

import React from 'react'
import * as ReactDOM from 'react-dom'
import { hot } from 'react-hot-loader/root'
import { unstable_trace as trace } from 'scheduler/tracing'
import create from 'zustand'
import {
  getProjectID,
  PROBABLY_ELECTRON,
  PRODUCTION_ENV,
  requireElectron,
} from '../common/env-vars'
import { EditorID } from '../core/shared/utils'
import CanvasActions from '../components/canvas/canvas-actions'
import { DispatchPriority, EditorAction, isLoggedIn } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorComponent } from '../components/editor/editor-component'
import * as History from '../components/editor/history'
import {
  InternalPreviewTimeout,
  previewIsAlive,
  startPreviewConnectedMonitoring,
} from '../components/editor/preview-report-handler'
import {
  downloadGithubRepo,
  getLoginState,
  getUserConfiguration,
  isRequestFailure,
  startPollingLoginState,
} from '../components/editor/server'
import {
  DispatchResult,
  editorDispatch,
  simpleStringifyActions,
} from '../components/editor/store/dispatch'
import {
  createEditorState,
  deriveState,
  EditorStore,
  getMainUIFromModel,
  defaultUserState,
  EditorState,
  DerivedState,
  UserState,
  PersistentModel,
  createNewProjectName,
  persistentModelForProjectContents,
} from '../components/editor/store/editor-state'
import {
  EditorStateContext,
  UtopiaStoreAPI,
  UtopiaStoreHook,
} from '../components/editor/store/store-hook'
import { RealBundlerWorker } from '../core/workers/bundler-bridge'
import { LinterResultMessage } from '../core/workers/linter/linter-worker'
import {
  RealLinterWorker,
  RealParserPrinterWorker,
  RealWatchdogWorker,
  UtopiaTsWorkersImplementation,
} from '../core/workers/workers'
import '../utils/react-shim'
import Utils from '../utils/utils'
import { HeartbeatRequestMessage } from '../core/workers/watchdog-worker'
import { triggerHashedAssetsUpdate } from '../utils/hashed-assets'
import {
  UiJsxCanvasContextData,
  emptyUiJsxCanvasContextData,
  UiJsxCanvasCtxAtom,
} from '../components/canvas/ui-jsx-canvas'
import { isLeft } from '../core/shared/either'
import { importZippedGitProject, isProjectImportSuccess } from '../core/model/project-import'
import { OutgoingWorkerMessage, UtopiaTsWorkers } from '../core/workers/common/worker-types'
import {
  isPropertyControlsIFrameReady,
  isSendPreviewModel,
  isUpdatePropertyControlsInfo,
  load,
} from '../components/editor/actions/actions'
import { updateCssVars, UtopiaStyles } from '../uuiui'
import { reduxDevtoolsSendInitialState } from '../core/shared/redux-devtools'
import { notice } from '../components/common/notice'
import { isCookiesOrLocalForageUnavailable, LoginState } from '../common/user'
import { PersistenceMachine } from '../components/editor/persistence/persistence'
import { PersistenceBackend } from '../components/editor/persistence/persistence-backend'
import { defaultProject } from '../sample-projects/sample-project-utils'

if (PROBABLY_ELECTRON) {
  let { webFrame } = requireElectron()
  webFrame.setVisualZoomLevelLimits(1, 1)
  webFrame.setLayoutZoomLevelLimits(0, 0)
}

function replaceLoadingMessage(newMessage: string) {
  const loadingMessageElement = document.getElementById('loading-message')
  if (loadingMessageElement != null) {
    loadingMessageElement.innerHTML = newMessage
  }
}

export class Editor {
  storedState: EditorStore
  utopiaStoreHook: UtopiaStoreHook
  utopiaStoreApi: UtopiaStoreAPI
  updateStore: (partialState: EditorStore) => void
  spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()

  constructor() {
    updateCssVars()
    startPreviewConnectedMonitoring(this.boundDispatch)

    let emptyEditorState = createEditorState(this.boundDispatch)
    const derivedState = deriveState(emptyEditorState, null)

    const history = History.init(emptyEditorState, derivedState)

    window.addEventListener('blur', this.resetStateOnBlur)

    window.addEventListener('message', this.onMessage)

    const watchdogWorker = new RealWatchdogWorker()

    const renderRootEditor = () =>
      renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi, this.spyCollector, true)

    const onCreatedOrLoadedProject = (
      projectId: string,
      projectName: string,
      project: PersistentModel,
    ) => load(this.storedState.dispatch, project, projectName, projectId)

    this.storedState = {
      editor: emptyEditorState,
      derived: derivedState,
      history: history,
      userState: defaultUserState,
      workers: new UtopiaTsWorkersImplementation(
        new RealParserPrinterWorker(),
        new RealLinterWorker(),
        watchdogWorker,
      ),
      persistence: new PersistenceMachine(
        PersistenceBackend,
        this.boundDispatch,
        renderProjectNotFound,
        onCreatedOrLoadedProject,
      ),
      dispatch: this.boundDispatch,
      alreadySaved: false,
    }

    const storeHook = create<EditorStore>((set) => this.storedState)

    this.utopiaStoreHook = storeHook
    this.updateStore = storeHook.setState
    this.utopiaStoreApi = storeHook

    renderRootEditor()

    reduxDevtoolsSendInitialState(this.storedState)

    const handleLinterMessage = (msg: LinterResultMessage) => {
      switch (msg.type) {
        case 'linterresult': {
          this.storedState.dispatch(
            [EditorActions.setCodeEditorLintErrors({ [msg.filename]: msg.errors })],
            'everyone',
          )
          break
        }
      }
    }

    const handleHeartbeatRequestMessage = (msg: HeartbeatRequestMessage) => {
      switch (msg.type) {
        case 'heartbeatrequest': {
          this.storedState.workers.sendHeartbeatResponseMessage(
            msg.id,
            msg.projectId,
            this.storedState.editor.safeMode,
          )
        }
      }
    }

    this.storedState.workers.addLinterResultEventListener((e) => handleLinterMessage(e.data))
    this.storedState.workers.addHeartbeatRequestEventListener((e) =>
      handleHeartbeatRequestMessage(e.data),
    )

    getLoginState('cache').then((loginState: LoginState) => {
      startPollingLoginState(this.boundDispatch, loginState)
      this.storedState.userState.loginState = loginState
      getUserConfiguration(loginState).then((shortcutConfiguration) => {
        this.storedState.userState = {
          ...this.storedState.userState,
          ...shortcutConfiguration,
        }

        const projectId = getProjectID()
        if (isLoggedIn(loginState)) {
          this.storedState.persistence.login()
        }

        const urlParams = new URLSearchParams(window.location.search)
        const githubOwner = urlParams.get('github_owner')
        const githubRepo = urlParams.get('github_repo')

        if (isCookiesOrLocalForageUnavailable(loginState)) {
          this.storedState.persistence.createNew(createNewProjectName(), defaultProject())
        } else if (projectId == null) {
          if (githubOwner != null && githubRepo != null) {
            replaceLoadingMessage('Downloading Repo...')

            downloadGithubRepo(githubOwner, githubRepo).then((repoResult) => {
              if (isRequestFailure(repoResult)) {
                if (repoResult.statusCode === 404) {
                  renderProjectNotFound()
                } else {
                  renderProjectLoadError(repoResult.errorMessage)
                }
              } else {
                replaceLoadingMessage('Importing Project...')

                const projectName = `${githubOwner}-${githubRepo}`
                importZippedGitProject(projectName, repoResult.value)
                  .then((importProjectResult) => {
                    if (isProjectImportSuccess(importProjectResult)) {
                      const importedProject = persistentModelForProjectContents(
                        importProjectResult.contents,
                      )
                      this.storedState.persistence.createNew(projectName, importedProject)
                    } else {
                      renderProjectLoadError(importProjectResult.errorMessage)
                    }
                  })
                  .catch((err) => {
                    console.error('Import error.', err)
                  })
              }
            })
          } else {
            this.storedState.persistence.createNew(emptyEditorState.projectName, defaultProject())
          }
        } else {
          this.storedState.persistence.load(projectId)
        }
      })
    })
  }

  onMessage = (event: MessageEvent): void => {
    const eventData = event.data
    if (isSendPreviewModel(eventData)) {
      previewIsAlive(InternalPreviewTimeout)
      this.boundDispatch([eventData], 'noone')
    } else if (isPropertyControlsIFrameReady(eventData)) {
      this.boundDispatch([eventData], 'noone')
    } else if (isUpdatePropertyControlsInfo(eventData)) {
      this.boundDispatch([eventData], 'noone')
    }
  }

  resetStateOnBlur = () => {
    this.boundDispatch(
      [
        EditorActions.clearHighlightedViews(),
        CanvasActions.clearDragState(false),
        EditorActions.updateKeys({}),
        EditorActions.closePopup(),
      ],
      'everyone',
    )
  }

  boundDispatch = (
    dispatchedActions: readonly EditorAction[],
    priority?: DispatchPriority,
  ): {
    entireUpdateFinished: Promise<any>
  } => {
    const runDispatch = () => {
      const result = editorDispatch(
        this.boundDispatch,
        dispatchedActions,
        this.storedState,
        this.spyCollector,
      )
      this.storedState = result

      if (!result.nothingChanged) {
        // we update the zustand store with the new editor state. this will trigger a re-render in the EditorComponent
        this.updateStore({
          ...result,
        })
      }
      return { entireUpdateFinished: result.entireUpdateFinished }
    }
    if (PRODUCTION_ENV) {
      return runDispatch()
    } else {
      return trace(
        `action-${dispatchedActions.map((a) => a.action)}`,
        performance.now(),
        runDispatch,
      )
    }
  }
}

export const EditorRoot: React.FunctionComponent<{
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
  spyCollector: UiJsxCanvasContextData
  propertyControlsInfoSupported: boolean
}> = ({ api, useStore, spyCollector, propertyControlsInfoSupported }) => {
  return (
    <EditorStateContext.Provider value={{ api, useStore }}>
      <UiJsxCanvasCtxAtom.Provider value={spyCollector}>
        <EditorComponent propertyControlsInfoSupported={propertyControlsInfoSupported} />
      </UiJsxCanvasCtxAtom.Provider>
    </EditorStateContext.Provider>
  )
}

EditorRoot.displayName = 'Utopia Editor Root'

export const HotRoot: React.FunctionComponent<{
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
  spyCollector: UiJsxCanvasContextData
  propertyControlsInfoSupported: boolean
}> = hot(({ api, useStore, spyCollector, propertyControlsInfoSupported }) => {
  return (
    <EditorRoot
      api={api}
      useStore={useStore}
      spyCollector={spyCollector}
      propertyControlsInfoSupported={propertyControlsInfoSupported}
    />
  )
})
HotRoot.displayName = 'Utopia Editor Hot Root'

async function renderRootComponent(
  useStore: UtopiaStoreHook,
  api: UtopiaStoreAPI,
  spyCollector: UiJsxCanvasContextData,
  propertyControlsInfoSupported: boolean,
): Promise<void> {
  return triggerHashedAssetsUpdate().then(() => {
    // NOTE: we only need to call this function once,
    // as subsequent updates will be fed through Zustand
    const rootElement = document.getElementById(EditorID)
    if (rootElement != null) {
      if (process.env.HOT_MODE) {
        ReactDOM.render(
          <HotRoot
            api={api}
            useStore={useStore}
            spyCollector={spyCollector}
            propertyControlsInfoSupported={propertyControlsInfoSupported}
          />,
          rootElement,
        )
      } else {
        ReactDOM.render(
          <EditorRoot
            api={api}
            useStore={useStore}
            spyCollector={spyCollector}
            propertyControlsInfoSupported={propertyControlsInfoSupported}
          />,
          rootElement,
        )
      }
    }
  })
}

const ProjectLoadError = ({ error }: { error: string }) => {
  return (
    <div
      style={{
        boxShadow: UtopiaStyles.shadowStyles.medium.boxShadow,
        borderRadius: 3,
        overflowWrap: 'break-word',
        wordWrap: 'break-word',
        hyphens: 'auto',
        whiteSpace: 'pre-wrap',
        maxWidth: 400,
        width: 400,
        padding: 12,
        fontWeight: 500,
        letterSpacing: 0.2,
        margin: '5px',
      }}
    >
      {error}
    </div>
  )
}

const renderProjectNotFound = () => renderProjectLoadError('Project could not be found.')

async function renderProjectLoadError(error: string): Promise<void> {
  const rootElement = document.getElementById(EditorID)
  if (rootElement != null) {
    ReactDOM.render(<ProjectLoadError error={error} />, rootElement)
  }
}
