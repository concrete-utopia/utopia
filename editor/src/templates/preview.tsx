import * as fastDeepEquals from 'fast-deep-equal'
import * as ReactErrorOverlay from 'react-error-overlay'
import { BASE_URL, getProjectID, getQueryParam, PREVIEW_IS_EMBEDDED } from '../common/env-vars'
import { fetchLocalProject } from '../common/persistence'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../components/assets'
import { incorporateBuildResult } from '../components/custom-code/code-file'
import { sendPreviewModel } from '../components/editor/actions/actions'
import { dependenciesWithEditorRequirements } from '../components/editor/npm-dependency/npm-dependency'
import { projectIsStoredLocally } from '../components/editor/persistence'
import { loadProject } from '../components/editor/server'
import { isProjectContentsUpdateMessage } from '../components/preview/preview-pane'
import { fetchNodeModules } from '../core/es-modules/package-manager/fetch-packages'
import { getRequireFn } from '../core/es-modules/package-manager/package-manager'
import { pluck } from '../core/shared/array-utils'
import { getMainHTMLFilename, getMainJSFilename } from '../core/shared/project-contents-utils'
import { isCodeFile, NodeModules } from '../core/shared/project-file-types'
import { NewBundlerWorker, RealBundlerWorker } from '../core/workers/bundler-bridge'
import { createBundle } from '../core/workers/bundler-promise'
import Utils from '../utils/utils'

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
      .model
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
    } catch (e) {
      onError(e)
    }
  }
  intervalHandle = window.setInterval(onTimeout, pollInterval)
  onTimeout()
}

function addOpenInUtopiaButton(): void {
  // Avoid doing this in the embedded preview, as it makes
  // no sense having it in there.
  if (getQueryParam('embedded') != 'true') {
    const projectId = getProjectID()
    if (projectId != null) {
      const openIcon = document.createElement('img')
      openIcon.setAttribute('src', `${BASE_URL}static/index/logo-smiangle-16x16@2x.png`)
      openIcon.setAttribute('style', 'width: 32px; height: 32px')
      const openText = document.createElement('span')
      openText.setAttribute('style', 'display: inline-flex; align-items: center; padding: 4px')
      openText.append('Open In Utopia')

      const openInUtopiaButton = document.createElement('div')
      const buttonStyle = [
        'position: fixed',
        'right: 20px',
        'bottom: 20px',
        'height: 32px',
        'border-color: darkgray',
        'border-width: medium',
        'border-style: solid',
        'display: flex',
        'flex-direction: row',
        'font-family: "Helvetica", "Arial", sans-serif',
        'cursor: pointer',
      ].join('; ')
      openInUtopiaButton.setAttribute('style', buttonStyle)
      openInUtopiaButton.setAttribute(
        'onclick',
        `window.open('${BASE_URL}project/${projectId}/', 'mywindow')`,
      )

      openInUtopiaButton.append(openIcon)
      openInUtopiaButton.append(openText)

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

  const startPollingFromServer = (appID: string | null) => {
    if (appID != null) {
      startPolledLoad({
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
      modelUpdated(eventData.projectContents)
    }
  }

  const handlePossiblyQueuedModel = () => {
    if (queuedModel == null) {
      loadingModel = false
    } else {
      renderProject(queuedModel)
    }
  }

  const renderProject = (projectContents: ProjectContentTreeRoot) => {
    loadingModel = true
    queuedModel = null
    previewRender(projectContents).finally(handlePossiblyQueuedModel)
  }

  const rerenderPreview = () => {
    if (lastRenderedModel != null) {
      previewRender(lastRenderedModel)
    }
  }

  const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
    moduleDownload.then((downloadedModules: NodeModules) => {
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
        const fetchNodeModulesResult = await fetchNodeModules(npmDependencies)

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

    incorporateBuildResult(nodeModules, bundledProjectFiles)

    const require = getRequireFn(onRemoteModuleDownload, projectContents, nodeModules)

    // replacing the document body first
    const previewHTMLFileName = getMainHTMLFilename(projectContents)
    const previewHTMLFile = getContentsTreeFileFromString(
      projectContents,
      `/${previewHTMLFileName}`,
    )
    if (previewHTMLFile != null && isCodeFile(previewHTMLFile)) {
      try {
        try {
          ReactErrorOverlay.stopReportingRuntimeErrors()
        } catch (e) {
          // we don't care
        }
        document.open()
        document.write(previewHTMLFile.fileContents)
        document.close()
        addOpenInUtopiaButton()
        addWindowListeners()
        ReactErrorOverlay.startReportingRuntimeErrors({})
      } catch (e) {
        console.warn(`no body found in html`, e)
      }
    }

    const previewJSFileName = getMainJSFilename(projectContents)
    const previewJSFilePath = `/${previewJSFileName}`
    const previewJSFile = getContentsTreeFileFromString(projectContents, previewJSFilePath)
    if (previewJSFile != null && isCodeFile(previewJSFile)) {
      if (bundledProjectFiles[previewJSFilePath] == null) {
        throw new Error(
          `Error processing the project files: the build result does not contain the preview file: ${previewJSFilePath}`,
        )
      } else {
        /**
         * require the js entry point file which will evaluate the module.
         * the React entry point js file traditionally has a top level side effect,
         * calling ReactDOM.render() which starts the Preview app.
         */
        require('/', previewJSFilePath)
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
