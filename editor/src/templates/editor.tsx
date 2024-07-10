// import feature switches so they are loaded before anything else can read them
import '../utils/feature-switches'

import React from 'react'
import * as PubSub from 'pubsub-js'
import { createRoot } from 'react-dom/client'
import * as ReactDOM from 'react-dom'
import { hot } from 'react-hot-loader/root'
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
import type {
  DispatchPriority,
  EditorAction,
  EditorDispatch,
} from '../components/editor/action-types'
import { isLoggedIn } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorComponent } from '../components/editor/editor-component'
import * as History from '../components/editor/history'
import {
  InternalPreviewTimeout,
  previewIsAlive,
  startPreviewConnectedMonitoring,
} from '../components/editor/preview-report-handler'
import {
  getLoginState,
  getUserConfiguration,
  getUserPermissions,
  startPollingLoginState,
} from '../components/editor/server'
import type { DispatchResult } from '../components/editor/store/dispatch'
import {
  editorDispatchActionRunner,
  editorDispatchClosingOut,
} from '../components/editor/store/dispatch'
import type {
  EditorStoreFull,
  PersistentModel,
  ElementsToRerender,
  GithubRepoWithBranch,
} from '../components/editor/store/editor-state'
import {
  createEditorState,
  deriveState,
  defaultUserState,
  createNewProjectName,
  patchedStoreFromFullStore,
  getCurrentTheme,
  emptyCollaborativeEditingSupport,
} from '../components/editor/store/editor-state'
import type { UtopiaStoreAPI } from '../components/editor/store/store-hook'
import {
  CanvasStateContext,
  createStoresAndState,
  EditorStateContext,
  LowPriorityStateContext,
  OriginalMainEditorStateContext,
} from '../components/editor/store/store-hook'
import type { LinterResultMessage } from '../core/workers/linter/linter-worker'
import {
  RealLinterWorker,
  RealParserPrinterWorker,
  RealWatchdogWorker,
  UtopiaTsWorkersImplementation,
} from '../core/workers/workers'
import '../utils/react-shim'
import type { HeartbeatRequestMessage } from '../core/workers/watchdog-worker'
import { triggerHashedAssetsUpdate } from '../utils/hashed-assets'
import type { UiJsxCanvasContextData } from '../components/canvas/ui-jsx-canvas'
import {
  emptyUiJsxCanvasContextData,
  UiJsxCanvasCtxAtom,
  ElementsToRerenderGLOBAL,
} from '../components/canvas/ui-jsx-canvas'
import { foldEither } from '../core/shared/either'
import { getURLImportDetails, reuploadAssets } from '../core/model/project-import'
import { isSendPreviewModel, load } from '../components/editor/actions/actions'
import { UtopiaStyles } from '../uuiui'
import { reduxDevtoolsSendInitialState } from '../core/shared/redux-devtools'
import type { LoginState } from '../common/user'
import { isCookiesOrLocalForageUnavailable } from '../common/user'
import { PersistenceMachine } from '../components/editor/persistence/persistence'
import { PersistenceBackend } from '../components/editor/persistence/persistence-backend'
import { defaultProject } from '../sample-projects/sample-project-utils'
import { createBuiltInDependenciesList } from '../core/es-modules/package-manager/built-in-dependencies-list'
import { createEmptyStrategyState } from '../components/canvas/canvas-strategies/interaction-state'
import type { DomWalkerMutableStateData } from '../components/canvas/dom-walker'
import {
  DomWalkerMutableStateCtx,
  createDomWalkerMutableState,
  invalidateDomWalkerIfNecessary,
} from '../components/canvas/dom-walker'
import { isFeatureEnabled } from '../utils/feature-switches'
import { shouldUpdateLowPriorityUI } from '../components/inspector/inspector'
import * as EP from '../core/shared/element-path'
import { waitUntil } from '../core/shared/promise-utils'
import { sendSetVSCodeTheme } from '../core/vscode/vscode-bridge'
import type { ElementPath } from '../core/shared/project-file-types'
import { uniqBy } from '../core/shared/array-utils'
import { updateUserDetailsWhenAuthenticated } from '../core/shared/github/helpers'
import { DispatchContext } from '../components/editor/store/dispatch-context'
import {
  logSelectorTimings,
  resetSelectorTimings,
} from '../components/editor/store/store-hook-performance-logging'
import { createPerformanceMeasure } from '../components/editor/store/editor-dispatch-performance-logging'
import { runDomWalkerAndSaveResults } from '../components/canvas/editor-dispatch-flow'
import { simpleStringifyActions } from '../components/editor/actions/action-utils'
import { unpatchedCreateRemixDerivedDataMemo } from '../components/editor/store/remix-derived-data'
import { emptyProjectServerState } from '../components/editor/store/project-server-state'
import { GithubAuth } from '../utils/github-auth'
import { Provider as JotaiProvider } from 'jotai'
import {
  getGithubRepoToLoad,
  LoadActionsDispatched,
} from '../components/github/github-repository-clone-flow'
import { hasReactRouterErrorBeenLogged } from '../core/shared/runtime-report-logs'
import { InitialOnlineState, startOnlineStatusPolling } from '../components/editor/online-status'
import { useAnimate } from 'framer-motion'
import { AnimationContext } from '../components/canvas/ui-jsx-canvas-renderer/animation-context'
import { anyCodeAhead } from '../components/assets'

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
    const derivedState = deriveState(
      emptyEditorState,
      null,
      'unpatched',
      unpatchedCreateRemixDerivedDataMemo,
    )

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
    const onCreatedOrLoadedProject = async (
      projectId: string,
      projectName: string,
      project: PersistentModel,
    ) => {
      await load(this.boundDispatch, project, projectName, projectId, builtInDependencies)
      PubSub.publish(LoadActionsDispatched, { projectId: projectId })
    }

    this.storedState = {
      unpatchedEditor: emptyEditorState,
      patchedEditor: emptyEditorState,
      unpatchedDerived: derivedState,
      patchedDerived: derivedState,
      strategyState: strategyState,
      postActionInteractionSession: null,
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
      projectServerState: emptyProjectServerState(),
      collaborativeEditingSupport: emptyCollaborativeEditingSupport(),
      onlineState: InitialOnlineState,
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

    this.domWalkerMutableState = createDomWalkerMutableState(
      this.utopiaStoreHook,
      this.boundDispatch,
    )

    void renderRootEditor()

    startOnlineStatusPolling(this.boundDispatch)

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
      const projectId = getProjectID()
      startPollingLoginState(this.boundDispatch, loginState)
      this.storedState.userState.loginState = loginState
      void Promise.all([
        getUserConfiguration(loginState),
        getUserPermissions(loginState, projectId),
      ]).then(([shortcutConfiguration, permissions]) => {
        const userState = {
          ...this.storedState.userState,
          ...shortcutConfiguration,
        }
        this.storedState.userState = userState

        // Ensure we have the correct theme set in VS Code
        sendSetVSCodeTheme(getCurrentTheme(userState))

        void updateUserDetailsWhenAuthenticated(
          this.boundDispatch,
          GithubAuth.isAuthenticatedWithGithub(loginState),
        ).then(async (authenticatedWithGithub) => {
          this.boundDispatch([
            EditorActions.setGithubState({
              authenticated: authenticatedWithGithub,
            }),
          ])
          if (isLoggedIn(loginState)) {
            this.storedState.persistence.login()
          }

          const urlParams = new URLSearchParams(window.location.search)
          const importURL = urlParams.get('import_url')

          const githubRepoToLoad: GithubRepoWithBranch | null = getGithubRepoToLoad(
            window.location.search,
          )

          if (isCookiesOrLocalForageUnavailable(loginState)) {
            this.storedState.persistence.createNew(createNewProjectName(), defaultProject())
          } else if (projectId == null) {
            if (githubRepoToLoad != null) {
              // by setting GithubState.gitRepoToLoad, we trigger github-clone-overlay.tsx which will take over the clone flow
              this.boundDispatch([
                EditorActions.setGithubState({
                  gitRepoToLoad: githubRepoToLoad,
                }),
              ])
              // TODO somehow make it a compile error if we don't give the control over to the component
            } else if (importURL != null) {
              this.createNewProjectFromImportURL(importURL)
            } else {
              await this.storedState.persistence.createNew(
                emptyEditorState.projectName,
                defaultProject(),
              )
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

  resetStateOnBlur = (): void => {
    this.boundDispatch(
      [
        EditorActions.clearHighlightedViews(),
        CanvasActions.clearInteractionSession(true),
        EditorActions.updateKeys({}),
        EditorActions.closePopup(),
        EditorActions.clearPostActionData(),
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
    const Measure = createPerformanceMeasure()
    Measure.logActions(dispatchedActions)

    const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')
    const PerformanceMarks =
      (isFeatureEnabled('Debug – Performance Marks (Slow)') ||
        isFeatureEnabled('Debug – Performance Marks (Fast)')) &&
      PERFORMANCE_MARKS_ALLOWED

    const runDispatch = () => {
      const oldEditorState = this.storedState

      const dispatchResult = editorDispatchActionRunner(
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

      const reactRouterErrorPreviouslyLogged = hasReactRouterErrorBeenLogged()

      const runDomWalker = shouldRunDOMWalker(dispatchedActions, dispatchResult)

      const somethingChanged = !dispatchResult.nothingChanged

      const shouldRerender =
        (somethingChanged || runDomWalker) &&
        !anyCodeAhead(dispatchResult.unpatchedEditor.projectContents)

      const updateId = canvasUpdateId++
      if (shouldRerender) {
        // TODO: not running this if nothing has changed in the result might give another performance benefit
        Measure.taskTime(`update canvas ${updateId}`, () => {
          const currentElementsToRender = fixElementsToRerender(
            this.storedState.patchedEditor.canvas.elementsToRerender,
            dispatchedActions,
          )
          ElementsToRerenderGLOBAL.current = currentElementsToRender // Mutation!
          ReactDOM.flushSync(() => {
            ReactDOM.unstable_batchedUpdates(() => {
              this.canvasStore.setState(patchedStoreFromFullStore(this.storedState, 'canvas-store'))
            })
          })
        })
      }

      // run the dom-walker
      if (runDomWalker) {
        const domWalkerDispatchResult = runDomWalkerAndSaveResults(
          this.boundDispatch,
          this.domWalkerMutableState,
          this.storedState,
          this.spyCollector,
          ElementsToRerenderGLOBAL.current,
        )

        if (domWalkerDispatchResult != null) {
          this.storedState = domWalkerDispatchResult
          entireUpdateFinished = Promise.all([
            entireUpdateFinished,
            domWalkerDispatchResult.entireUpdateFinished,
          ])
        }
      }

      // true up groups if needed
      if (this.storedState.unpatchedEditor.trueUpElementsAfterDomWalkerRuns.length > 0) {
        // updated editor with trued up groups
        Measure.taskTime(`Group true up ${updateId}`, () => {
          const projectContentsBeforeGroupTrueUp = this.storedState.unpatchedEditor.projectContents
          const dispatchResultWithTruedUpGroups = editorDispatchActionRunner(
            this.boundDispatch,
            [{ action: 'TRUE_UP_ELEMENTS' }],
            this.storedState,
            this.spyCollector,
          )
          this.storedState = dispatchResultWithTruedUpGroups

          entireUpdateFinished = Promise.all([
            entireUpdateFinished,
            dispatchResultWithTruedUpGroups.entireUpdateFinished,
          ])

          if (
            projectContentsBeforeGroupTrueUp === this.storedState.unpatchedEditor.projectContents
          ) {
            // no group-related re-render / re-measure is needed, bail out
            return
          }

          // re-render the canvas
          Measure.taskTime(`Canvas re-render because of groups ${updateId}`, () => {
            ElementsToRerenderGLOBAL.current = fixElementsToRerender(
              this.storedState.patchedEditor.canvas.elementsToRerender,
              dispatchedActions,
            ) // Mutation!

            ReactDOM.flushSync(() => {
              ReactDOM.unstable_batchedUpdates(() => {
                this.canvasStore.setState(
                  patchedStoreFromFullStore(this.storedState, 'canvas-store'),
                )
              })
            })
          })

          // re-run the dom-walker
          Measure.taskTime(`Dom walker re-run because of groups ${updateId}`, () => {
            const domWalkerDispatchResult = runDomWalkerAndSaveResults(
              this.boundDispatch,
              this.domWalkerMutableState,
              this.storedState,
              this.spyCollector,
              ElementsToRerenderGLOBAL.current,
            )

            if (domWalkerDispatchResult != null) {
              this.storedState = domWalkerDispatchResult
              entireUpdateFinished = Promise.all([
                entireUpdateFinished,
                domWalkerDispatchResult.entireUpdateFinished,
              ])
            }
          })
        })
      }

      if (somethingChanged) {
        this.storedState = editorDispatchClosingOut(
          this.boundDispatch,
          dispatchedActions,
          oldEditorState,
          {
            ...this.storedState,
            entireUpdateFinished: entireUpdateFinished,
            nothingChanged: dispatchResult.nothingChanged,
          },
          reactRouterErrorPreviouslyLogged,
        )
      }

      Measure.taskTime(`Update Editor ${updateId}`, () => {
        ReactDOM.flushSync(() => {
          ReactDOM.unstable_batchedUpdates(() => {
            Measure.taskTime(`Update Main Store ${updateId}`, () => {
              this.utopiaStoreHook.setState(
                patchedStoreFromFullStore(this.storedState, 'editor-store'),
              )
            })

            if (
              shouldUpdateLowPriorityUI(
                this.storedState.strategyState,
                ElementsToRerenderGLOBAL.current,
              )
            ) {
              Measure.taskTime(`Update Low Prio Store ${updateId}`, () => {
                this.lowPriorityStore.setState(
                  patchedStoreFromFullStore(this.storedState, 'low-priority-store'),
                )
              })
            }
            if (MeasureSelectors) {
              logSelectorTimings('store update phase')
            }
            if (PerformanceMarks) {
              performance.mark(`react wrap up ${updateId}`)
            }

            // reset selector timings right before the end of flushSync means we'll capture the re-render related selector data with a clean slate
            resetSelectorTimings()

            if (PerformanceMarks) {
              performance.measure(
                `Our Components Rendering + React Doing Stuff`,
                `react wrap up ${updateId}`,
              )
            }
          })
        })
      })

      return {
        entireUpdateFinished: entireUpdateFinished,
      }
    }
    resetSelectorTimings()
    const result = Measure.taskTime(
      `Editor Dispatch ${simpleStringifyActions(dispatchedActions)}`,
      () => {
        return runDispatch()
      },
    )
    if (MeasureSelectors) {
      logSelectorTimings('re-render phase')
    }
    Measure.printMeasurements()

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
  const [animationScope, animate] = useAnimate()

  return (
    <AtomsDevtools>
      <JotaiProvider>
        <DispatchContext.Provider value={dispatch}>
          <OriginalMainEditorStateContext.Provider value={mainStore}>
            <EditorStateContext.Provider value={mainStore}>
              <DomWalkerMutableStateCtx.Provider value={domWalkerMutableState}>
                <CanvasStateContext.Provider value={canvasStore}>
                  <LowPriorityStateContext.Provider value={lowPriorityStore}>
                    <UiJsxCanvasCtxAtom.Provider value={spyCollector}>
                      <AnimationContext.Provider
                        value={{ animate: animate, scope: animationScope }}
                      >
                        <EditorComponent />
                      </AnimationContext.Provider>
                    </UiJsxCanvasCtxAtom.Provider>
                  </LowPriorityStateContext.Provider>
                </CanvasStateContext.Provider>
              </DomWalkerMutableStateCtx.Provider>
            </EditorStateContext.Provider>
          </OriginalMainEditorStateContext.Provider>
        </DispatchContext.Provider>
      </JotaiProvider>
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
      const root = createRoot(rootElement)
      if (process.env.HOT_MODE != null) {
        root.render(
          <HotRoot
            dispatch={dispatch}
            mainStore={mainStore}
            spyCollector={spyCollector}
            canvasStore={canvasStore}
            lowPriorityStore={lowPriorityStore}
            domWalkerMutableState={domWalkerMutableState}
          />,
        )
      } else {
        root.render(
          <EditorRoot
            dispatch={dispatch}
            spyCollector={spyCollector}
            mainStore={mainStore}
            canvasStore={canvasStore}
            lowPriorityStore={lowPriorityStore}
            domWalkerMutableState={domWalkerMutableState}
          />,
        )
      }
    }
  })
}

const ProjectLoadError = ({ error }: { error: string }) => {
  return (
    <div
      style={{
        boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
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

async function renderProjectLoadError(error: string): Promise<void> {
  const rootElement = document.getElementById(EditorID)
  if (rootElement != null) {
    const root = createRoot(rootElement)
    root.render(<ProjectLoadError error={error} />)
  }
}

async function renderProjectNotFound(projectId: string): Promise<void> {
  window.location.href = `/project/${projectId}`
}

function shouldRunDOMWalker(
  dispatchedActions: readonly EditorAction[],
  dispatchResult: DispatchResult,
): boolean {
  // if the only thing that changed was the scroll position - we don't need to run the DOMWalker
  // TODO: make it a more robust check on the contents of the result - or even better - add a specific flag for 'domContentsChanged'
  const canSkipDomWalker = dispatchedActions.every((a) => a.action === 'SCROLL_CANVAS')
  if (canSkipDomWalker) {
    return false
  }
  return (
    !dispatchResult.nothingChanged || dispatchedActions.some((a) => a.action === 'RUN_DOM_WALKER')
  )
}
