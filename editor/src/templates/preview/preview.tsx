import '../../vite-shims'
import fastDeepEquals from 'fast-deep-equal'
import * as ReactErrorOverlay from 'react-error-overlay'
import { BASE_URL, getProjectID, getQueryParam, PREVIEW_IS_EMBEDDED } from '../../common/env-vars'
import { fetchLocalProject } from '../../common/persistence'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { getProjectFileByFilePath } from '../../components/assets'
import { incorporateBuildResult } from '../../components/custom-code/code-file'
import { sendPreviewModel } from '../../components/editor/actions/action-creators'
import { dependenciesWithEditorRequirements } from '../../components/editor/npm-dependency/npm-dependency'
import { projectIsStoredLocally } from '../../components/editor/persistence/persistence-backend'
import { loadProject } from '../../components/editor/server'
import { isProjectContentsUpdateMessage } from '../../components/preview/preview-pane'
import { createBuiltInDependenciesList } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { fetchNodeModules } from '../../core/es-modules/package-manager/fetch-packages'
import {
  getRequireFn,
  ResolvingRemoteDependencyErrorName,
} from '../../core/es-modules/package-manager/package-manager'
import { pluck } from '../../core/shared/array-utils'
import { getMainHTMLFilename, getMainJSFilename } from '../../core/shared/project-contents-utils'
import type { NodeModules } from '../../core/shared/project-file-types'
import { isTextFile } from '../../core/shared/project-file-types'
import { injectTwind } from '../../core/tailwind/tailwind'
import { NewBundlerWorker, RealBundlerWorker } from '../../core/workers/bundler-bridge'
import { createBundle } from '../../core/workers/bundler-promise'
import Utils from '../../utils/utils'

interface PolledLoadParams {
  projectId: string
  onStartLoad: () => void
  onModelChanged: (model: ProjectContentTreeRoot) => void
  onModelUnchanged: () => void
  onError: (error: Error) => void
}

export async function startPolledLoad({
  projectId,
  onStartLoad,
  onModelChanged,
  onModelUnchanged,
  onError,
}: PolledLoadParams) {
  let keepPolling = true
  let isLocal = await projectIsStoredLocally(projectId)
  let pollInterval = isLocal ? 250 : 1000

  let lastSavedTS: string | null = null
  let isLoading = false

  const loadServerProject = async () => {
    if (!isLoading) {
      isLoading = true
      onStartLoad()
      const project = await loadProject(projectId, lastSavedTS)
      if (project.type === 'ProjectLoaded') {
        // eslint-disable-next-line require-atomic-updates
        lastSavedTS = project.modifiedAt
        onModelChanged(project.content.projectContents)
      } else if (project.type === 'ProjectUnchanged') {
        onModelUnchanged()
      }
      // eslint-disable-next-line require-atomic-updates
      isLoading = false
    }
  }

  const loadLocalProject = async () => {
    const project = await fetchLocalProject(projectId)
    const model = Utils.forceNotNull(`Local project ${projectId} could not be found.`, project)
      .model.projectContents
    onModelChanged(model)
  }

  let intervalHandle: number | null = null

  async function onTimeout() {
    // eslint-disable-next-line require-atomic-updates
    isLocal = isLocal && (await projectIsStoredLocally(projectId))
    pollInterval = isLocal ? 250 : 1000

    const polledLoad = isLocal ? loadLocalProject : loadServerProject
    try {
      if (keepPolling) {
        await polledLoad()
      } else if (intervalHandle != null) {
        window.clearInterval(intervalHandle)
      }
    } catch (e: any) {
      onError(e)
    }
  }
  intervalHandle = window.setInterval(onTimeout, pollInterval)
  void onTimeout()
}

function addOpenInUtopiaButton(): void {
  // Avoid doing this in the embedded preview, as it makes
  // no sense having it in there.
  if (getQueryParam('embedded') != 'true') {
    const projectId = getProjectID()
    if (projectId != null) {
      const openIcon = document.createElement('img')
      openIcon.setAttribute('src', `${BASE_URL}open_in_utopia@2x.png`)
      openIcon.setAttribute('style', 'width: 186px; height: 40px')
      openIcon.setAttribute('alt', 'Open this project in the Utopia editor')

      const openInUtopiaButton = document.createElement('div')
      const buttonStyle = [
        'position: fixed',
        'right: 20px',
        'bottom: 20px',
        'cursor: pointer',
      ].join('; ')
      openInUtopiaButton.setAttribute('style', buttonStyle)
      openInUtopiaButton.setAttribute(
        'onclick',
        `window.open('${BASE_URL}project/${projectId}/', 'mywindow')`,
      )

      openInUtopiaButton.append(openIcon)

      document.body.append(openInUtopiaButton)
    }
  }
}

let lastRenderedModel: ProjectContentTreeRoot | null = null
let queuedModel: ProjectContentTreeRoot | null = null
let loadingModel: boolean = false
let cachedDependencies: NodeModules = {}

const initPreview = () => {
  loadingModel = false
  queuedModel = null
  cachedDependencies = {}
  const bundlerWorker = new NewBundlerWorker(new RealBundlerWorker())
  const builtInDependencies = createBuiltInDependenciesList(null)

  const startPollingFromServer = (appID: string | null) => {
    if (appID != null) {
      void startPolledLoad({
        projectId: appID,
        // eslint-disable-next-line @typescript-eslint/no-empty-function
        onStartLoad: () => {},
        onModelChanged: modelUpdated,
        // eslint-disable-next-line @typescript-eslint/no-empty-function
        onModelUnchanged: () => {},
        onError: (error) => {
          console.error('received error', error, error.message)
        },
      })
    }
  }

  const modelUpdated = async (model: ProjectContentTreeRoot) => {
    if (loadingModel) {
      queuedModel = model
    } else {
      renderProject(model)
    }
  }

  const handleModelUpdateEvent = (event: MessageEvent) => {
    // Received a model from the containing page.
    const eventData = event.data
    if (isProjectContentsUpdateMessage(eventData)) {
      void modelUpdated(eventData.projectContents)
    }
  }

  const handlePossiblyQueuedModel = () => {
    if (queuedModel == null) {
      loadingModel = false
    } else {
      renderProject(queuedModel)
    }
  }

  const renderProject = (projectContents: ProjectContentTreeRoot): void => {
    loadingModel = true
    queuedModel = null
    void previewRender(projectContents).finally(handlePossiblyQueuedModel)
  }

  const rerenderPreview = () => {
    if (lastRenderedModel != null) {
      void previewRender(lastRenderedModel)
    }
  }

  const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
    void moduleDownload.then((downloadedModules: NodeModules) => {
      // MUTATION
      Object.assign(cachedDependencies, downloadedModules)
      rerenderPreview()
    })
  }

  const previewRender = async (projectContents: ProjectContentTreeRoot) => {
    const isRerender = projectContents === lastRenderedModel
    let nodeModules: NodeModules = {}

    if (isRerender) {
      nodeModules = { ...cachedDependencies }
    } else {
      const lastDependencies =
        lastRenderedModel == null ? [] : dependenciesWithEditorRequirements(lastRenderedModel)
      const npmDependencies = dependenciesWithEditorRequirements(projectContents)
      if (fastDeepEquals(lastDependencies, npmDependencies)) {
        nodeModules = { ...cachedDependencies }
      } else {
        const fetchNodeModulesResult = await fetchNodeModules(
          npmDependencies,
          builtInDependencies,
          true,
        )

        if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
          const errorToThrow = Error(
            `Could not load the following npm dependencies: ${JSON.stringify(
              pluck(fetchNodeModulesResult.dependenciesWithError, 'name'),
            )}`,
          )
          ReactErrorOverlay.reportRuntimeError(errorToThrow)
          throw errorToThrow
        }

        cachedDependencies = {
          ...cachedDependencies,
          ...fetchNodeModulesResult.nodeModules,
        }
        nodeModules = { ...cachedDependencies }
      }
    }

    lastRenderedModel = projectContents

    /**
     * please note that we are passing in an empty object instead of the .d.ts files
     * the reason for this is that we only use the bundler as a transpiler here
     * and we don't care about static type errors
     *
     * some libraries, ie Antd have so massive definitions that it took more than 20 seconds
     * for the bundler to process it, basically with no upside
     */
    const emptyTypeDefinitions = {}
    const bundledProjectFiles = (
      await createBundle(bundlerWorker, emptyTypeDefinitions, projectContents)
    ).buildResult

    incorporateBuildResult(nodeModules, projectContents, bundledProjectFiles)

    const requireFn = getRequireFn(
      onRemoteModuleDownload,
      projectContents,
      nodeModules,
      {},
      builtInDependencies,
    )

    // replacing the document body first
    const previewHTMLFileName = getMainHTMLFilename(projectContents)
    const previewHTMLFile = getProjectFileByFilePath(projectContents, `/${previewHTMLFileName}`)
    if (previewHTMLFile != null && isTextFile(previewHTMLFile)) {
      try {
        try {
          ReactErrorOverlay.stopReportingRuntimeErrors()
        } catch (e) {
          // we don't care
        }
        document.open()
        document.write(previewHTMLFile.fileContents.code)
        document.close()
        addOpenInUtopiaButton()
        addWindowListeners()
        ReactErrorOverlay.startReportingRuntimeErrors({})
      } catch (e) {
        console.warn(`no body found in html`, e)
      }
    }

    injectTwind(projectContents, requireFn, '')

    const previewJSFileName = getMainJSFilename(projectContents)
    const previewJSFilePath = `/${previewJSFileName}`
    const previewJSFile = getProjectFileByFilePath(projectContents, previewJSFilePath)
    if (previewJSFile != null && isTextFile(previewJSFile)) {
      if (bundledProjectFiles[previewJSFilePath] == null) {
        throw new Error(
          `Error processing the project files: the build result does not contain the preview file: ${previewJSFilePath}`,
        )
      } else {
        try {
          /**
           * require the js entry point file which will evaluate the module.
           * the React entry point js file traditionally has a top level side effect,
           * calling ReactDOM.render() which starts the Preview app.
           */
          requireFn('/', previewJSFilePath)
        } catch (e: any) {
          if (e?.name === ResolvingRemoteDependencyErrorName && e?.message != null) {
            const loadingMessageDiv = document.createElement('div')
            loadingMessageDiv.innerHTML = e.message
            const loadingMessageStyle = [
              `font-family: -apple-system, BlinkMacSystemFont, Helvetica, 'Segoe UI', Roboto, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'`,
              'font-size: 11px',
              'cursor: default',
            ].join('; ')
            loadingMessageDiv.setAttribute('style', loadingMessageStyle)
            document.body.append(loadingMessageDiv)
          } else {
            throw e
          }
        }
      }
    } else {
      throw new Error(
        `Error processing the project files: the preview path (${previewJSFilePath}) did not point to a valid file`,
      )
    }
  }

  const addWindowListeners = () => {
    window.addEventListener('message', handleModelUpdateEvent)
  }

  const loadPreviewContent = () => {
    const projectId = getProjectID()
    addWindowListeners()

    if (PREVIEW_IS_EMBEDDED) {
      // Tell the editor we'd like to be sent the initial model.
      window.parent.postMessage(sendPreviewModel(), '*')
    } else {
      startPollingFromServer(projectId)
    }
  }
  loadPreviewContent()
}
initPreview()
