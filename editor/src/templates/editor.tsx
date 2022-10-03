// import feature switches so they are loaded before anything else can read them
import '../utils/feature-switches'

import React from 'react'
import * as ReactDOM from 'react-dom'
import { hot } from 'react-hot-loader/root'
import { unstable_trace as trace } from 'scheduler/tracing'
import create, { GetState, Mutate, SetState, StoreApi } from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import '../utils/vite-hmr-config'
import {
  getProjectID,
  PERFORMANCE_MARKS_ALLOWED,
  PROBABLY_ELECTRON,
  PRODUCTION_ENV,
  requireElectron,
} from '../common/env-vars'
import { arrayEquals, EditorID } from '../core/shared/utils'
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
  EditorStoreFull,
  getMainUIFromModel,
  defaultUserState,
  EditorState,
  DerivedState,
  UserState,
  PersistentModel,
  createNewProjectName,
  persistentModelForProjectContents,
  EditorStorePatched,
  patchedStoreFromFullStore,
  ElementsToRerender,
} from '../components/editor/store/editor-state'
import {
  CanvasStateContext,
  EditorStateContext,
  InspectorStateContext,
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
import { HeartbeatRequestMessage } from '../core/workers/watchdog-worker'
import { triggerHashedAssetsUpdate } from '../utils/hashed-assets'
import {
  UiJsxCanvasContextData,
  emptyUiJsxCanvasContextData,
  UiJsxCanvasCtxAtom,
  ElementsToRerenderGLOBAL,
} from '../components/canvas/ui-jsx-canvas'
import { foldEither, isLeft } from '../core/shared/either'
import {
  getURLImportDetails,
  importZippedGitProject,
  isProjectImportSuccess,
  reuploadAssets,
} from '../core/model/project-import'
import { OutgoingWorkerMessage, UtopiaTsWorkers } from '../core/workers/common/worker-types'
import { isSendPreviewModel, load } from '../components/editor/actions/actions'
import { updateCssVars, UtopiaStyles } from '../uuiui'
import { reduxDevtoolsSendInitialState } from '../core/shared/redux-devtools'
import { notice } from '../components/common/notice'
import { isCookiesOrLocalForageUnavailable, LoginState } from '../common/user'
import { PersistenceMachine } from '../components/editor/persistence/persistence'
import { PersistenceBackend } from '../components/editor/persistence/persistence-backend'
import { defaultProject } from '../sample-projects/sample-project-utils'
import { createBuiltInDependenciesList } from '../core/es-modules/package-manager/built-in-dependencies-list'
import { createEmptyStrategyState } from '../components/canvas/canvas-strategies/interaction-state'
import {
  DomWalkerMutableStateCtx,
  DomWalkerMutableStateData,
  createDomWalkerMutableState,
  initDomWalkerObservers,
  invalidateDomWalkerIfNecessary,
  runDomWalker,
} from '../components/canvas/dom-walker'
import { isFeatureEnabled } from '../utils/feature-switches'
import { shouldInspectorUpdate } from '../components/inspector/inspector'
import * as EP from '../core/shared/element-path'
import { isAuthenticatedWithGithub } from '../utils/github-auth'
import { ProjectContentTreeRootKeepDeepEquality } from '../components/editor/store/store-deep-equality-instances'
import { waitUntil } from '../core/shared/promise-utils'

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

// If the elements to re-render have specific paths in 2 consecutive passes, but those paths differ, then
// for this pass treat it as `rerender-all-elements`, to ensure that the metadata gets cleaned up as
// the previously focused elements may not now exist.
// Also as some canvas strategies may not supply a specific set of elements to re-render, if
// `rerender-all-elements` switches to a specific set of paths, ignore the specific set of paths
// for the very first pass to get another `rerender-all-elements`.
let lastElementsToRerender: ElementsToRerender = 'rerender-all-elements'
function fixElementsToRerender(currentElementsToRerender: ElementsToRerender): ElementsToRerender {
  let elementsToRerender: ElementsToRerender = currentElementsToRerender
  switch (lastElementsToRerender) {
    case 'rerender-all-elements':
      switch (currentElementsToRerender) {
        case 'rerender-all-elements':
          break
        default:
          elementsToRerender = 'rerender-all-elements'
      }
      break
    default:
      switch (currentElementsToRerender) {
        case 'rerender-all-elements':
          break
        default:
          if (!arrayEquals(lastElementsToRerender, currentElementsToRerender, EP.pathsEqual)) {
            elementsToRerender = 'rerender-all-elements'
          }
      }
  }
  lastElementsToRerender = currentElementsToRerender
  return elementsToRerender
}

export class Editor {
  storedState: EditorStoreFull
  utopiaStoreHook: UtopiaStoreHook
  utopiaStoreApi: UtopiaStoreAPI
  updateStore: (partialState: EditorStorePatched) => void
  canvasStore: UtopiaStoreHook & UtopiaStoreAPI
  updateCanvasStore: (partialState: EditorStorePatched) => void
  inspectorStore: UtopiaStoreHook & UtopiaStoreAPI
  updateInspectorStore: (partialState: EditorStorePatched) => void
  spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()
  domWalkerMutableState: DomWalkerMutableStateData

  constructor() {
    updateCssVars()
    startPreviewConnectedMonitoring(this.boundDispatch)

    let emptyEditorState = createEditorState(this.boundDispatch)
    const derivedState = deriveState(emptyEditorState, null)

    const strategyState = createEmptyStrategyState()

    const history = History.init(emptyEditorState, derivedState)

    window.addEventListener('blur', this.resetStateOnBlur)

    window.addEventListener('message', this.onMessage)

    const watchdogWorker = new RealWatchdogWorker()

    const renderRootEditor = () =>
      renderRootComponent(
        this.utopiaStoreHook,
        this.utopiaStoreApi,
        this.canvasStore,
        this.inspectorStore,
        this.spyCollector,
        this.domWalkerMutableState,
      )

    const workers = new UtopiaTsWorkersImplementation(
      new RealParserPrinterWorker(),
      new RealLinterWorker(),
      watchdogWorker,
    )

    const builtInDependencies = createBuiltInDependenciesList(workers)
    const onCreatedOrLoadedProject = (
      projectId: string,
      projectName: string,
      project: PersistentModel,
    ) => load(this.storedState.dispatch, project, projectName, projectId, builtInDependencies)

    this.storedState = {
      unpatchedEditor: emptyEditorState,
      patchedEditor: emptyEditorState,
      unpatchedDerived: derivedState,
      patchedDerived: derivedState,
      strategyState: strategyState,
      history: history,
      userState: defaultUserState,
      workers: workers,
      persistence: new PersistenceMachine(
        PersistenceBackend,
        this.boundDispatch,
        renderProjectNotFound,
        onCreatedOrLoadedProject,
      ),
      dispatch: this.boundDispatch,
      builtInDependencies: builtInDependencies,
      alreadySaved: false,
    }

    const storeHook = create<
      EditorStorePatched,
      SetState<EditorStorePatched>,
      GetState<EditorStorePatched>,
      Mutate<StoreApi<EditorStorePatched>, [['zustand/subscribeWithSelector', never]]>
    >(subscribeWithSelector((set) => patchedStoreFromFullStore(this.storedState)))

    const canvasStoreHook = create<
      EditorStorePatched,
      SetState<EditorStorePatched>,
      GetState<EditorStorePatched>,
      Mutate<StoreApi<EditorStorePatched>, [['zustand/subscribeWithSelector', never]]>
    >(subscribeWithSelector((set) => patchedStoreFromFullStore(this.storedState)))

    const inspectorStoreHook = create<
      EditorStorePatched,
      SetState<EditorStorePatched>,
      GetState<EditorStorePatched>,
      Mutate<StoreApi<EditorStorePatched>, [['zustand/subscribeWithSelector', never]]>
    >(subscribeWithSelector((set) => patchedStoreFromFullStore(this.storedState)))

    this.utopiaStoreHook = storeHook
    this.updateStore = storeHook.setState
    this.utopiaStoreApi = storeHook

    this.canvasStore = canvasStoreHook
    this.updateCanvasStore = canvasStoreHook.setState

    this.inspectorStore = inspectorStoreHook
    this.updateInspectorStore = inspectorStoreHook.setState

    this.domWalkerMutableState = createDomWalkerMutableState(this.utopiaStoreApi)

    void renderRootEditor()

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
            this.storedState.patchedEditor.safeMode,
          )
        }
      }
    }

    this.storedState.workers.addLinterResultEventListener((e) => handleLinterMessage(e.data))
    this.storedState.workers.addHeartbeatRequestEventListener((e) =>
      handleHeartbeatRequestMessage(e.data),
    )

    void getLoginState('cache').then((loginState: LoginState) => {
      startPollingLoginState(this.boundDispatch, loginState)
      this.storedState.userState.loginState = loginState
      void getUserConfiguration(loginState).then((shortcutConfiguration) => {
        this.storedState.userState = {
          ...this.storedState.userState,
          ...shortcutConfiguration,
        }

        void isAuthenticatedWithGithub(loginState).then((authenticatedWithGithub) => {
          this.storedState.userState = {
            ...this.storedState.userState,
            githubState: {
              authenticated: authenticatedWithGithub,
            },
          }
          const projectId = getProjectID()
          if (isLoggedIn(loginState)) {
            this.storedState.persistence.login()
          }

          const urlParams = new URLSearchParams(window.location.search)
          const githubOwner = urlParams.get('github_owner')
          const githubRepo = urlParams.get('github_repo')
          const importURL = urlParams.get('import_url')

          if (isCookiesOrLocalForageUnavailable(loginState)) {
            this.storedState.persistence.createNew(createNewProjectName(), defaultProject())
          } else if (projectId == null) {
            if (githubOwner != null && githubRepo != null) {
              replaceLoadingMessage('Downloading Repo...')

              void downloadGithubRepo(githubOwner, githubRepo).then((repoResult) => {
                if (isRequestFailure(repoResult)) {
                  if (repoResult.statusCode === 404) {
                    void renderProjectNotFound()
                  } else {
                    void renderProjectLoadError(repoResult.errorMessage)
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
                        void renderProjectLoadError(importProjectResult.errorMessage)
                      }
                    })
                    .catch((err) => {
                      console.error('Import error.', err)
                    })
                }
              })
            } else if (importURL != null) {
              this.createNewProjectFromImportURL(importURL)
            } else {
              this.storedState.persistence.createNew(emptyEditorState.projectName, defaultProject())
            }
          } else {
            this.storedState.persistence.load(projectId)
          }
        })
      })
    })
  }

  onMessage = (event: MessageEvent): void => {
    const eventData = event.data
    if (isSendPreviewModel(eventData)) {
      previewIsAlive(InternalPreviewTimeout)
      this.boundDispatch([eventData], 'noone')
    }
  }

  resetStateOnBlur = () => {
    this.boundDispatch(
      [
        EditorActions.clearHighlightedViews(),
        CanvasActions.clearDragState(false),
        CanvasActions.clearInteractionSession(false),
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
      const PerformanceMarks =
        isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED

      const oldEditorState = this.storedState

      const dispatchResult = editorDispatch(
        this.boundDispatch,
        dispatchedActions,
        oldEditorState,
        this.spyCollector,
      )

      invalidateDomWalkerIfNecessary(
        this.domWalkerMutableState,
        oldEditorState.patchedEditor,
        dispatchResult.patchedEditor,
      )

      this.storedState = dispatchResult
      let entireUpdateFinished = dispatchResult.entireUpdateFinished

      if (!dispatchResult.nothingChanged) {
        const updateId = canvasUpdateId++
        // we update the zustand store with the new editor state. this will trigger a re-render in the EditorComponent
        if (PerformanceMarks) {
          performance.mark(`update canvas ${updateId}`)
        }
        const currentElementsToRender = fixElementsToRerender(
          this.storedState.patchedEditor.canvas.elementsToRerender,
        )
        ElementsToRerenderGLOBAL.current = currentElementsToRender // Mutation!
        ReactDOM.flushSync(() => {
          ReactDOM.unstable_batchedUpdates(() => {
            this.updateCanvasStore(patchedStoreFromFullStore(this.storedState))
          })
        })
        if (PerformanceMarks) {
          performance.mark(`update canvas end ${updateId}`)
          performance.measure(
            `Update Canvas ${updateId} – [${
              typeof ElementsToRerenderGLOBAL.current === 'string'
                ? ElementsToRerenderGLOBAL.current
                : ElementsToRerenderGLOBAL.current.map(EP.toString).join(', ')
            }]`,
            `update canvas ${updateId}`,
            `update canvas end ${updateId}`,
          )
        }

        const domWalkerResult = runDomWalker({
          domWalkerMutableState: this.domWalkerMutableState,
          selectedViews: this.storedState.patchedEditor.selectedViews,
          elementsToFocusOn: currentElementsToRender,
          scale: this.storedState.patchedEditor.canvas.scale,
          additionalElementsToUpdate:
            this.storedState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
          rootMetadataInStateRef: {
            current: this.storedState.patchedEditor.domMetadata,
          },
        })

        if (domWalkerResult != null) {
          const dispatchResultWithMetadata = editorDispatch(
            this.boundDispatch,
            [
              EditorActions.saveDOMReport(
                domWalkerResult.metadata,
                domWalkerResult.cachedPaths,
                domWalkerResult.invalidatedPaths,
              ),
            ],
            this.storedState,
            this.spyCollector,
          )
          this.storedState = dispatchResultWithMetadata
          entireUpdateFinished = Promise.all([
            entireUpdateFinished,
            dispatchResultWithMetadata.entireUpdateFinished,
          ])
        }

        if (PerformanceMarks) {
          performance.mark(`update editor ${updateId}`)
        }
        ReactDOM.flushSync(() => {
          ReactDOM.unstable_batchedUpdates(() => {
            this.updateStore(patchedStoreFromFullStore(this.storedState))
            if (shouldInspectorUpdate(this.storedState.strategyState)) {
              this.updateInspectorStore(patchedStoreFromFullStore(this.storedState))
            }
          })
        })
        if (PerformanceMarks) {
          performance.mark(`update editor end ${updateId}`)
          performance.measure(
            `Update Editor ${updateId}`,
            `update editor ${updateId}`,
            `update editor end ${updateId}`,
          )
        }
      }

      return {
        entireUpdateFinished: entireUpdateFinished,
      }
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

  private createNewProjectFromImportURL(importURL: string): void {
    getURLImportDetails(importURL)
      .then((importResult) => {
        foldEither(
          (errorMessage) => {
            void renderProjectLoadError(errorMessage)
          },
          (successResult) => {
            // Create the new project.
            this.storedState.persistence.createNew(createNewProjectName(), successResult.model)

            // Checks to see if the given project has been loaded and received a project ID.
            function projectLoaded(storedState: EditorStoreFull): boolean {
              const successResultFilenames = new Set(
                Object.keys(successResult.model.projectContents),
              )
              const unpatchedEditorFilenames = new Set(
                Object.keys(storedState.unpatchedEditor.projectContents),
              )
              return (
                successResultFilenames.size === unpatchedEditorFilenames.size &&
                storedState.unpatchedEditor.id != null
              )
            }

            void waitUntil(5000, () => projectLoaded(this.storedState)).then((waitResult) => {
              if (waitResult) {
                void reuploadAssets(
                  successResult.originalProjectRootURL,
                  this.storedState.unpatchedEditor,
                )
              } else {
                console.error(`Waited too long for the project to get a app ID.`)
              }
            })
          },
          importResult,
        )
      })
      .catch((err) => {
        console.error('Import error.', err)
      })
  }
}

let canvasUpdateId: number = 0

export const EditorRoot: React.FunctionComponent<{
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
  canvasStore: UtopiaStoreAPI & UtopiaStoreHook
  inspectorStore: UtopiaStoreAPI & UtopiaStoreHook
  spyCollector: UiJsxCanvasContextData
  domWalkerMutableState: DomWalkerMutableStateData
}> = ({ api, useStore, canvasStore, inspectorStore, spyCollector, domWalkerMutableState }) => {
  return (
    <EditorStateContext.Provider value={{ api, useStore }}>
      <DomWalkerMutableStateCtx.Provider value={domWalkerMutableState}>
        <CanvasStateContext.Provider value={{ api: canvasStore, useStore: canvasStore }}>
          <InspectorStateContext.Provider value={{ api: inspectorStore, useStore: inspectorStore }}>
            <UiJsxCanvasCtxAtom.Provider value={spyCollector}>
              <EditorComponent />
            </UiJsxCanvasCtxAtom.Provider>
          </InspectorStateContext.Provider>
        </CanvasStateContext.Provider>
      </DomWalkerMutableStateCtx.Provider>
    </EditorStateContext.Provider>
  )
}

EditorRoot.displayName = 'Utopia Editor Root'

export const HotRoot: React.FunctionComponent<{
  api: UtopiaStoreAPI
  useStore: UtopiaStoreHook
  canvasStore: UtopiaStoreAPI & UtopiaStoreHook
  inspectorStore: UtopiaStoreAPI & UtopiaStoreHook
  spyCollector: UiJsxCanvasContextData
  domWalkerMutableState: DomWalkerMutableStateData
}> = hot(({ api, useStore, canvasStore, inspectorStore, spyCollector, domWalkerMutableState }) => {
  return (
    <EditorRoot
      api={api}
      useStore={useStore}
      spyCollector={spyCollector}
      canvasStore={canvasStore}
      inspectorStore={inspectorStore}
      domWalkerMutableState={domWalkerMutableState}
    />
  )
})
HotRoot.displayName = 'Utopia Editor Hot Root'

async function renderRootComponent(
  useStore: UtopiaStoreHook,
  api: UtopiaStoreAPI,
  canvasStore: UtopiaStoreAPI & UtopiaStoreHook,
  inspectorStore: UtopiaStoreAPI & UtopiaStoreHook,
  spyCollector: UiJsxCanvasContextData,
  domWalkerMutableState: DomWalkerMutableStateData,
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
            canvasStore={canvasStore}
            inspectorStore={inspectorStore}
            domWalkerMutableState={domWalkerMutableState}
          />,
          rootElement,
        )
      } else {
        ReactDOM.render(
          <EditorRoot
            api={api}
            useStore={useStore}
            spyCollector={spyCollector}
            canvasStore={canvasStore}
            inspectorStore={inspectorStore}
            domWalkerMutableState={domWalkerMutableState}
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
