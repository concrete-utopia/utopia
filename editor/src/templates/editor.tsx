// import feature switches so they are loaded before anything else can read them
import '../utils/feature-switches'

import React from 'react'
import * as ReactDOM from 'react-dom'
import { hot } from 'react-hot-loader/root'
import { unstable_trace as trace } from 'scheduler/tracing'
import { useAtomsDevtools } from 'jotai-devtools'
import '../utils/vite-hmr-config'
import {
  getProjectID,
  PERFORMANCE_MARKS_ALLOWED,
  PROBABLY_ELECTRON,
  PRODUCTION_ENV,
  requireElectron,
} from '../common/env-vars'
import { EditorID } from '../core/shared/utils'
import CanvasActions from '../components/canvas/canvas-actions'
import {
  DispatchPriority,
  EditorAction,
  EditorDispatch,
  isLoggedIn,
} from '../components/editor/action-types'
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
  getCurrentTheme,
} from '../components/editor/store/editor-state'
import {
  CanvasStateContext,
  createStoresAndState,
  EditorStateContext,
  LowPriorityStateContext,
  OriginalMainEditorStateContext,
  UtopiaStores,
  UtopiaStoreAPI,
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
import { UtopiaStyles } from '../uuiui'
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
import { shouldInspectorUpdate as shouldUpdateLowPriorityUI } from '../components/inspector/inspector'
import * as EP from '../core/shared/element-path'
import { isAuthenticatedWithGithub } from '../utils/github-auth'
import { ProjectContentTreeRootKeepDeepEquality } from '../components/editor/store/store-deep-equality-instances'
import { waitUntil } from '../core/shared/promise-utils'
import { sendSetVSCodeTheme } from '../core/vscode/vscode-bridge'
import { ElementPath } from '../core/shared/project-file-types'
import { uniqBy } from '../core/shared/array-utils'
import {
  refreshGithubData,
  updateUserDetailsWhenAuthenticated,
} from '../core/shared/github/helpers'
import { DispatchContext } from '../components/editor/store/dispatch-context'
import {
  logSelectorTimings,
  resetSelectorTimings,
} from '../components/editor/store/store-hook-performance-logging'

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

function collectElementsToRerenderForTransientActions(
  working: Array<ElementPath>,
  action: EditorAction,
): Array<ElementPath> {
  if (action.action === 'TRANSIENT_ACTIONS') {
    if (action.elementsToRerender != null) {
      working.push(...action.elementsToRerender)
    }
    working.push(
      ...action.transientActions.reduce(collectElementsToRerenderForTransientActions, working),
    )
    return working
  } else {
    return working
  }
}

// If the elements to re-render have specific paths in 2 consecutive passes, but those paths differ, then
// for this pass use a union of the two arrays, to make sure we clear a previously focused element from the metadata
// and let the canvas re-render components that may have a missing child now.
let lastElementsToRerender: ElementsToRerender = 'rerender-all-elements'
function fixElementsToRerender(
  currentElementsToRerender: ElementsToRerender,
  dispatchedActions: readonly EditorAction[],
): ElementsToRerender {
  // while running transient actions there is an optional elementsToRerender
  const elementsToRerenderTransient = uniqBy<ElementPath>(
    dispatchedActions.reduce(
      collectElementsToRerenderForTransientActions,
      [] as Array<ElementPath>,
    ),
    EP.pathsEqual,
  )

  const currentOrTransientElementsToRerender =
    elementsToRerenderTransient.length > 0 ? elementsToRerenderTransient : currentElementsToRerender

  let fixedElementsToRerender: ElementsToRerender = currentOrTransientElementsToRerender
  if (
    currentOrTransientElementsToRerender !== 'rerender-all-elements' &&
    lastElementsToRerender !== 'rerender-all-elements'
  ) {
    // if the current elements to rerender array doesn't match the previous elements to rerender array, for a single frame let's use the union of the two arrays
    fixedElementsToRerender = EP.uniqueElementPaths([
      ...lastElementsToRerender,
      ...currentOrTransientElementsToRerender,
    ])
  }

  lastElementsToRerender = currentOrTransientElementsToRerender
  return fixedElementsToRerender
}

const GITHUB_REFRESH_INTERVAL_MILLISECONDS = 30_000

function startGithubPolling(utopiaStoreAPI: UtopiaStoreAPI, dispatch: EditorDispatch): void {
  function pollGithub(): void {
    try {
      const currentState = utopiaStoreAPI.getState()
      const githubAuthenticated = currentState.userState.githubState.authenticated
      const githubRepo = currentState.editor.githubSettings.targetRepository
      const branchName = currentState.editor.githubSettings.branchName
      const githubChecksums = currentState.derived.githubChecksums
      const githubUserDetails = currentState.editor.githubData.githubUserDetails
      const lastRefreshedCommit = currentState.editor.githubData.lastRefreshedCommit
      void refreshGithubData(
        dispatch,
        githubAuthenticated,
        githubRepo,
        branchName,
        githubChecksums,
        githubUserDetails,
        lastRefreshedCommit,
      )
    } finally {
      // Trigger another one to run Xms _after_ this has finished.
      globalThis.setTimeout(pollGithub, GITHUB_REFRESH_INTERVAL_MILLISECONDS)
    }
  }

  // Trigger a poll initially.
  pollGithub()
}

export class Editor {
  storedState: EditorStoreFull
  utopiaStoreHook: UtopiaStoreAPI
  canvasStore: UtopiaStoreAPI
  lowPriorityStore: UtopiaStoreAPI
  spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()
  domWalkerMutableState: DomWalkerMutableStateData

  constructor() {
    startPreviewConnectedMonitoring(this.boundDispatch)

    let emptyEditorState = createEditorState(this.boundDispatch)
    const derivedState = deriveState(emptyEditorState, null)

    const strategyState = createEmptyStrategyState({}, {}, {})

    const history = History.init(emptyEditorState, derivedState)

    window.addEventListener('blur', this.resetStateOnBlur)

    window.addEventListener('message', this.onMessage)

    const watchdogWorker = new RealWatchdogWorker()

    const renderRootEditor = () =>
      renderRootComponent(
        this.boundDispatch,
        this.utopiaStoreHook,
        this.canvasStore,
        this.lowPriorityStore,
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
    ) => load(this.boundDispatch, project, projectName, projectId, builtInDependencies)

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
      builtInDependencies: builtInDependencies,
      saveCountThisSession: 0,
    }

    const store = createStoresAndState(patchedStoreFromFullStore(this.storedState, 'editor-store'))

    const canvasStore = createStoresAndState(
      patchedStoreFromFullStore(this.storedState, 'canvas-store'),
    )

    const lowPriorityStore = createStoresAndState(
      patchedStoreFromFullStore(this.storedState, 'low-priority-store'),
    )

    this.utopiaStoreHook = store

    this.canvasStore = canvasStore

    this.lowPriorityStore = lowPriorityStore

    this.domWalkerMutableState = createDomWalkerMutableState(this.utopiaStoreHook)

    void renderRootEditor()

    startGithubPolling(this.utopiaStoreHook, this.boundDispatch)

    reduxDevtoolsSendInitialState(this.storedState)

    const handleLinterMessage = (msg: LinterResultMessage) => {
      switch (msg.type) {
        case 'linterresult': {
          this.boundDispatch(
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
        const userState = {
          ...this.storedState.userState,
          ...shortcutConfiguration,
        }
        this.storedState.userState = userState

        // Ensure we have the correct theme set in VS Code
        sendSetVSCodeTheme(getCurrentTheme(userState))

        void updateUserDetailsWhenAuthenticated(
          this.boundDispatch,
          isAuthenticatedWithGithub(loginState),
        ).then((authenticatedWithGithub) => {
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
    const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')
    if (MeasureSelectors) {
      // eslint-disable-next-line no-console
      console.log('------------------')
    }
    const PerformanceMarks =
      (isFeatureEnabled('Debug – Performance Marks (Slow)') ||
        isFeatureEnabled('Debug – Performance Marks (Fast)')) &&
      PERFORMANCE_MARKS_ALLOWED

    const runDispatch = () => {
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
          dispatchedActions,
        )
        ElementsToRerenderGLOBAL.current = currentElementsToRender // Mutation!
        const beforeCanvasStore = MeasureSelectors ? performance.now() : 0
        ReactDOM.flushSync(() => {
          ReactDOM.unstable_batchedUpdates(() => {
            this.canvasStore.setState(patchedStoreFromFullStore(this.storedState, 'canvas-store'))
          })
        })
        const afterCanvasStore = MeasureSelectors ? performance.now() : 0
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
            if (PerformanceMarks) {
              performance.mark(`update main store ${updateId}`)
            }
            const beforeMainStore = MeasureSelectors ? performance.now() : 0
            this.utopiaStoreHook.setState(
              patchedStoreFromFullStore(this.storedState, 'editor-store'),
            )
            const afterMainStore = MeasureSelectors ? performance.now() : 0

            if (PerformanceMarks) {
              performance.measure(`Update Main Store ${updateId}`, `update main store ${updateId}`)
            }

            if (
              shouldUpdateLowPriorityUI(this.storedState.strategyState, currentElementsToRender)
            ) {
              if (PerformanceMarks) {
                performance.mark(`update low priority store ${updateId}`)
              }
              this.lowPriorityStore.setState(
                patchedStoreFromFullStore(this.storedState, 'low-priority-store'),
              )
              if (PerformanceMarks) {
                performance.measure(
                  `Update Low Prio Store ${updateId}`,
                  `update low priority store ${updateId}`,
                )
              }
            }
            const afterStoreUpdate = MeasureSelectors ? performance.now() : 0
            if (MeasureSelectors) {
              console.info(
                'Dispatched actions:',
                simpleStringifyActions(dispatchedActions),
                'All stores',
                afterStoreUpdate - beforeCanvasStore,
                'Canvas store',
                afterCanvasStore - beforeCanvasStore,
                'main store',
                afterMainStore - beforeMainStore,
                'slow store',
                afterStoreUpdate - afterMainStore,
              )
              logSelectorTimings('store update phase')
            }
            if (PerformanceMarks) {
              performance.mark(`react wrap up ${updateId}`)
            }

            // reset selector timings right before the end of flushSync means we'll capture the re-render related selector data with a clean slate
            resetSelectorTimings()
          })
        })
        if (PerformanceMarks) {
          performance.measure(
            `Our Components Rendering + React Doing Stuff`,
            `react wrap up ${updateId}`,
          )
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
    resetSelectorTimings()
    if (PerformanceMarks) {
      performance.mark('beforeFullDispatch')
    }
    const result = runDispatch()
    if (MeasureSelectors) {
      logSelectorTimings('re-render phase')
    }
    if (PerformanceMarks) {
      performance.measure(
        `Editor Dispatch ${simpleStringifyActions(dispatchedActions)}`,
        'beforeFullDispatch',
      )
    }
    return result
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

const AtomsDevtools = (props: { children: React.ReactNode }) => {
  if (!PRODUCTION_ENV) {
    // eslint-disable-next-line react-hooks/rules-of-hooks
    useAtomsDevtools(`Utopia Jotai Atoms Debug Store`)
  }
  return <>{props.children}</>
}

export const EditorRoot: React.FunctionComponent<{
  dispatch: EditorDispatch
  mainStore: UtopiaStoreAPI
  canvasStore: UtopiaStoreAPI
  lowPriorityStore: UtopiaStoreAPI
  spyCollector: UiJsxCanvasContextData
  domWalkerMutableState: DomWalkerMutableStateData
}> = ({
  dispatch,
  mainStore,
  canvasStore,
  lowPriorityStore,
  spyCollector,
  domWalkerMutableState,
}) => {
  return (
    <AtomsDevtools>
      <DispatchContext.Provider value={dispatch}>
        <OriginalMainEditorStateContext.Provider value={mainStore}>
          <EditorStateContext.Provider value={mainStore}>
            <DomWalkerMutableStateCtx.Provider value={domWalkerMutableState}>
              <CanvasStateContext.Provider value={canvasStore}>
                <LowPriorityStateContext.Provider value={lowPriorityStore}>
                  <UiJsxCanvasCtxAtom.Provider value={spyCollector}>
                    <EditorComponent />
                  </UiJsxCanvasCtxAtom.Provider>
                </LowPriorityStateContext.Provider>
              </CanvasStateContext.Provider>
            </DomWalkerMutableStateCtx.Provider>
          </EditorStateContext.Provider>
        </OriginalMainEditorStateContext.Provider>
      </DispatchContext.Provider>
    </AtomsDevtools>
  )
}

EditorRoot.displayName = 'Utopia Editor Root'

export const HotRoot: React.FunctionComponent<{
  dispatch: EditorDispatch
  mainStore: UtopiaStoreAPI
  canvasStore: UtopiaStoreAPI
  lowPriorityStore: UtopiaStoreAPI
  spyCollector: UiJsxCanvasContextData
  domWalkerMutableState: DomWalkerMutableStateData
}> = hot(
  ({ dispatch, mainStore, canvasStore, lowPriorityStore, spyCollector, domWalkerMutableState }) => {
    return (
      <EditorRoot
        dispatch={dispatch}
        spyCollector={spyCollector}
        mainStore={mainStore}
        canvasStore={canvasStore}
        lowPriorityStore={lowPriorityStore}
        domWalkerMutableState={domWalkerMutableState}
      />
    )
  },
)
HotRoot.displayName = 'Utopia Editor Hot Root'

async function renderRootComponent(
  dispatch: EditorDispatch,
  mainStore: UtopiaStoreAPI,
  canvasStore: UtopiaStoreAPI,
  lowPriorityStore: UtopiaStoreAPI,
  spyCollector: UiJsxCanvasContextData,
  domWalkerMutableState: DomWalkerMutableStateData,
): Promise<void> {
  return triggerHashedAssetsUpdate().then(() => {
    // NOTE: we only need to call this function once,
    // as subsequent updates will be fed through Zustand
    const rootElement = document.getElementById(EditorID)
    if (rootElement != null) {
      if (process.env.HOT_MODE != null) {
        ReactDOM.render(
          <HotRoot
            dispatch={dispatch}
            mainStore={mainStore}
            spyCollector={spyCollector}
            canvasStore={canvasStore}
            lowPriorityStore={lowPriorityStore}
            domWalkerMutableState={domWalkerMutableState}
          />,
          rootElement,
        )
      } else {
        ReactDOM.render(
          <EditorRoot
            dispatch={dispatch}
            spyCollector={spyCollector}
            mainStore={mainStore}
            canvasStore={canvasStore}
            lowPriorityStore={lowPriorityStore}
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
