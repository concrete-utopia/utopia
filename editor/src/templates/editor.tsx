import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { hot } from 'react-hot-loader/root'
import { unstable_trace as trace } from 'scheduler/tracing'
import { updateCssVars } from 'uuiui'
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
import { getLoginState } from '../components/editor/server'
import { editorDispatch, simpleStringifyActions } from '../components/editor/store/dispatch'
import {
  createEditorState,
  deriveState,
  EditorStore,
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
import { dependenciesFromProjectContents } from '../components/editor/npm-dependency/npm-dependency'

if (PROBABLY_ELECTRON) {
  let { webFrame } = requireElectron()
  webFrame.setVisualZoomLevelLimits(1, 1)
  webFrame.setLayoutZoomLevelLimits(0, 0)
}

export class Editor {
  storedState: EditorStore
  utopiaStoreHook: UtopiaStoreHook
  utopiaStoreApi: UtopiaStoreAPI
  updateStore: (partialState: EditorStore) => void
  boundDispatch: EditorDispatch = this.dispatch.bind(this)

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
      loginState: notLoggedIn,
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
          let updateCodeResultCacheAction: Array<EditorAction> = []
          if (!this.storedState.editor.safeMode) {
            const codeResultCache = generateCodeResultCache(
              msg.buildResult,
              msg.exportsInfo,
              this.storedState.editor.nodeModules.files,
              this.boundDispatch,
              dependenciesFromProjectContents(this.storedState.editor.projectContents),
              msg.fullBuild,
            )

            updateCodeResultCacheAction = Utils.maybeToArray(codeResultCache).map(
              EditorActions.updateCodeResultCache,
            )
          }
          const errors = getAllErrorsFromBuildResult(msg.buildResult)
          const actions = updateCodeResultCacheAction.concat(
            EditorActions.setCodeEditorBuildErrors(errors),
          )
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
      this.storedState.loginState = loginState

      const projectId = getProjectID()
      if (projectId == null) {
        createNewProject(this.boundDispatch, () =>
          renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi),
        )
      } else if (isSampleProject(projectId)) {
        createNewProjectFromSampleProject(
          projectId,
          this.boundDispatch,
          this.storedState.workers,
          () => renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi),
        )
      } else {
        projectIsStoredLocally(projectId).then((isLocal) => {
          if (isLocal) {
            loadFromLocalStorage(
              projectId,
              this.boundDispatch,
              isLoggedIn(loginState),
              this.storedState.workers,
              () => renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi),
            )
          } else {
            loadFromServer(projectId, this.boundDispatch, this.storedState.workers, () =>
              renderRootComponent(this.utopiaStoreHook, this.utopiaStoreApi),
            )
          }
        })
      }
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
      const result = editorDispatch(this.boundDispatch, dispatchedActions, this.storedState)
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
}> = hot(({ api, useStore }) => {
  return (
    <EditorStateContext.Provider value={{ api, useStore }}>
      <EditorComponent />
    </EditorStateContext.Provider>
  )
})
HotRoot.displayName = 'Utopia Editor Root'

function renderRootComponent(useStore: UtopiaStoreHook, api: UtopiaStoreAPI): Promise<void> {
  return triggerHashedAssetsUpdate().then(() => {
    // NOTE: we only need to call this function once,
    // as subsequent updates will be fed through Zustand
    const rootElement = document.getElementById(EditorID)
    if (rootElement != null) {
      ReactDOM.render(<HotRoot api={api} useStore={useStore} />, rootElement)
    }
  })
}

window.addEventListener('error', (error) => {
  console.error('⚠️ uncaught error! \n' + error.error.stack.split('\n').slice(0, 10).join('\n'))
  // TODO somehow let our tester know that there was an uncaught error
})
