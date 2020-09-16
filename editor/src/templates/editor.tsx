import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { hot } from 'react-hot-loader/root'
import { unstable_trace as trace } from 'scheduler/tracing'
import { updateCssVars, UtopiaStyles } from 'uuiui'
import create from 'zustand'
import {
  getProjectID,
  PROBABLY_ELECTRON,
  PRODUCTION_ENV,
  requireElectron,
} from '../common/env-vars'
import { EditorID } from '../core/shared/utils'
import CanvasActions from '../components/canvas/canvas-actions'
import { CodeResultCache, generateCodeResultCache } from '../components/custom-code/code-file'
import { getAllErrorsFromBuildResult } from '../components/custom-code/custom-code-utils'
import {
  EditorAction,
  EditorDispatch,
  isLoggedIn,
  notLoggedIn,
} from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/actions'
import { EditorComponent } from '../components/editor/editor-component'
import * as History from '../components/editor/history'
import {
  createNewProject,
  createNewProjectFromImportedProject,
  createNewProjectFromSampleProject,
  loadFromLocalStorage,
  loadFromServer,
  projectIsStoredLocally,
} from '../components/editor/persistence'
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
} from '../components/editor/server'
import { editorDispatch, simpleStringifyActions } from '../components/editor/store/dispatch'
import {
  createEditorState,
  deriveState,
  EditorStore,
  getMainUIFromModel,
  defaultUserState,
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
import { isSampleProject } from '../sample-projects/sample-project-utils'
import { OutgoingWorkerMessage } from '../core/workers/ts/ts-worker'
import '../utils/react-shim'
import Utils from '../utils/utils'
import { HeartbeatRequestMessage } from '../core/workers/watchdog-worker'
import { triggerHashedAssetsUpdate } from '../utils/hashed-assets'
import { getRequireFn } from '../core/es-modules/package-manager/package-manager'
import { dependenciesWithEditorRequirements } from '../components/editor/npm-dependency/npm-dependency'
import {
  UiJsxCanvasContextData,
  emptyUiJsxCanvasContextData,
  UiJsxCanvasContext,
} from '../components/canvas/ui-jsx-canvas'
import { isLeft } from '../core/shared/either'
import { importZippedGitProject, isProjectImportSuccess } from '../core/model/project-import'

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
  boundDispatch: EditorDispatch = this.dispatch.bind(this)
  spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()

  constructor() {
    updateCssVars()
    startPreviewConnectedMonitoring(this.boundDispatch)

    let emptyEditorState = createEditorState(this.boundDispatch)
    const fromScratchResult = deriveState(emptyEditorState, null, false)
    emptyEditorState = fromScratchResult.editor
    const derivedState = fromScratchResult.derived

    const history = History.init(emptyEditorState, derivedState)

    window.addEventListener('blur', this.resetStateOnBlur)

    window.addEventListener('message', this.onMessage)

    const watchdogWorker = new RealWatchdogWorker()

    this.storedState = {
      editor: emptyEditorState,
      derived: derivedState,
      history: history,
      userState: defaultUserState,
      workers: new UtopiaTsWorkersImplementation(
        new RealBundlerWorker(),
        new RealParserPrinterWorker(),
        new RealLinterWorker(),
        watchdogWorker,
      ),
      dispatch: this.boundDispatch,
    }

    const [storeHook, api] = create<EditorStore>((set) => this.storedState)

    this.utopiaStoreHook = storeHook
    this.updateStore = api.setState
    this.utopiaStoreApi = api

    const handleWorkerMessage = (msg: OutgoingWorkerMessage) => {
      switch (msg.type) {
        case 'build': {
          // FIXME: In theory, we could get back an error from an old build here, which would no longer be
          // applicable, but since we now clear those on code changes, and failed builds are extremely fast,
          // this is near impossible to reproduce *right now*. However, it could still become a very real
          // issue in the future, so it still needs addressing
          let actions: Array<EditorAction> = []
          if (!this.storedState.editor.safeMode) {
            const codeResultCache = generateCodeResultCache(
              this.storedState.editor.codeResultCache.projectModules,
              msg.buildResult,
              msg.exportsInfo,
              this.storedState.editor.nodeModules.files,
              this.boundDispatch,
              dependenciesWithEditorRequirements(this.storedState.editor.projectContents),
              msg.buildType,
              getMainUIFromModel(this.storedState.editor),
            )

            if (codeResultCache != null) {
              actions.push(EditorActions.updateCodeResultCache(codeResultCache, msg.buildType))
            }
          }
          const errors = getAllErrorsFromBuildResult(msg.buildResult)
          actions.push(EditorActions.setCodeEditorBuildErrors(errors))
          this.storedState.dispatch(actions, 'everyone')
          break
        }
      }
    }

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

    this.storedState.workers.addBundleResultEventListener((e) => handleWorkerMessage(e.data))
    this.storedState.workers.addLinterResultEventListener((e) => handleLinterMessage(e.data))
    this.storedState.workers.addHeartbeatRequestEventListener((e) =>
      handleHeartbeatRequestMessage(e.data),
    )

    getLoginState().then((loginState) => {
      this.storedState.userState.loginState = loginState
      getUserConfiguration(loginState).then((shortcutConfiguration) => {
        this.storedState.userState = {
          ...this.storedState.userState,
          ...shortcutConfiguration,
        }

        const projectId = getProjectID()
        if (projectId == null) {
          // Check if this is a github import
          const urlParams = new URLSearchParams(window.location.search)
          const githubOwner = urlParams.get('github_owner')
          const githubRepo = urlParams.get('github_repo')
          if (isLoggedIn(loginState) && githubOwner != null && githubRepo != null) {
            // TODO Should we require users to be logged in for this?
            downloadGithubRepo(githubOwner, githubRepo).then((repoResult) => {
              if (isRequestFailure(repoResult)) {
                if (repoResult.statusCode === 404) {
                  renderProjectNotFound()
                } else {
                  renderProjectLoadError(repoResult.errorMessage)
                }
              } else {
                const projectName = `${githubOwner}-${githubRepo}`
                replaceLoadingMessage('Downloading Repo...')
                importZippedGitProject(projectName, repoResult.value).then(
                  (importProjectResult) => {
                    if (isProjectImportSuccess(importProjectResult)) {
                      replaceLoadingMessage('Importing Project...')
                      createNewProjectFromImportedProject(
                        importProjectResult,
                        this.storedState.workers,
                        this.boundDispatch,
                        () =>
                          renderRootComponent(
                            this.utopiaStoreHook,
                            this.utopiaStoreApi,
                            this.spyCollector,
                          ),
                      )
                    } else {
                      renderProjectLoadError(importProjectResult.errorMessage)
                    }
                  },
                )
              }
            })
          } else {
            if (githubOwner != null && githubRepo != null) {
              this.boundDispatch(
                [
                  EditorActions.showToast({
                    message: 'Please log in to fork a github repo',
                  }),
                ],
                'everyone',
              )
            }

            createNewProject(this.boundDispatch, () =>
              renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi, this.spyCollector),
            )
          }
        } else if (isSampleProject(projectId)) {
          createNewProjectFromSampleProject(
            projectId,
            this.boundDispatch,
            this.storedState.workers,
            () => renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi, this.spyCollector),
          )
        } else {
          projectIsStoredLocally(projectId).then((isLocal) => {
            if (isLocal) {
              loadFromLocalStorage(
                projectId,
                this.boundDispatch,
                isLoggedIn(loginState),
                this.storedState.workers,
                () =>
                  renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi, this.spyCollector),
              )
            } else {
              loadFromServer(
                projectId,
                this.boundDispatch,
                this.storedState.workers,
                () => {
                  renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi, this.spyCollector)
                },
                () => {
                  renderProjectNotFound()
                },
              )
            }
          })
        }
      })
    })
  }

  onMessage = (event: MessageEvent) => {
    const eventData = event.data
    if (EditorActions.isSendPreviewModel(eventData)) {
      previewIsAlive(InternalPreviewTimeout)
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

  dispatch(dispatchedActions: readonly EditorAction[]) {
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
    }
    if (PRODUCTION_ENV) {
      runDispatch()
    } else {
      trace(`action-${dispatchedActions.map((a) => a.action)}`, performance.now(), runDispatch)
    }
  }
}

export const HotRoot: React.FunctionComponent<{
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
  spyCollector: UiJsxCanvasContextData
}> = hot(({ api, useStore, spyCollector }) => {
  return (
    <EditorStateContext.Provider value={{ api, useStore }}>
      <UiJsxCanvasContext.Provider value={spyCollector}>
        <EditorComponent />
      </UiJsxCanvasContext.Provider>
    </EditorStateContext.Provider>
  )
})
HotRoot.displayName = 'Utopia Editor Root'

async function renderRootComponent(
  useStore: UtopiaStoreHook,
  api: UtopiaStoreAPI,
  spyCollector: UiJsxCanvasContextData,
): Promise<void> {
  return triggerHashedAssetsUpdate().then(() => {
    // NOTE: we only need to call this function once,
    // as subsequent updates will be fed through Zustand
    const rootElement = document.getElementById(EditorID)
    if (rootElement != null) {
      ReactDOM.render(
        <HotRoot api={api} useStore={useStore} spyCollector={spyCollector} />,
        rootElement,
      )
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
