import * as json5 from 'json5'
import * as R from 'ramda'
import * as ReactErrorOverlay from 'react-error-overlay'
import { getProjectID, PREVIEW_IS_EMBEDDED, BASE_URL, getQueryParam } from '../common/env-vars'
import { fetchLocalProject } from '../common/persistence'
import { sendPreviewModel } from '../components/editor/actions/actions'
import { dependenciesFromProjectContents } from '../components/editor/npm-dependency/npm-dependency'
import { projectIsStoredLocally } from '../components/editor/persistence'
import { loadProject } from '../components/editor/server'
import { isPersistentModel, PersistentModel } from '../components/editor/store/editor-state'
import Utils from '../utils/utils'
import { isCodeFile, ESCodeFile, esCodeFile, NodeModules } from '../core/shared/project-file-types'
import type { ProjectContents } from '../core/shared/project-file-types'
import {
  getRequireFn,
  getDependencyTypeDefinitions,
} from '../core/es-modules/package-manager/package-manager'
import { npmDependency } from '../core/shared/npm-dependency-types'
import { objectMap } from '../core/shared/object-utils'
import { fetchNodeModules } from '../core/es-modules/package-manager/fetch-packages'
import { createBundle } from '../core/workers/bundler-promise'
import { NewBundlerWorker, RealBundlerWorker } from '../core/workers/bundler-bridge'
import { fastForEach } from '../core/shared/utils'
import { incorporateBuildResult } from '../components/custom-code/code-file'
import { PRODUCTION_CONFIG } from '../core/shared/detect-env'

interface PolledLoadParams {
  projectId: string
  onStartLoad: () => void
  onModelChanged: (model: PersistentModel | null) => void
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
        onModelChanged(project.content)
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
      openIcon.setAttribute(
        'src',
        `${BASE_URL(PRODUCTION_CONFIG)}static/index/logo-smiangle-16x16@2x.png`,
      )
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
        `window.open('${BASE_URL(PRODUCTION_CONFIG)}project/${projectId}/', 'mywindow')`,
      )

      openInUtopiaButton.append(openIcon)
      openInUtopiaButton.append(openText)

      document.body.append(openInUtopiaButton)
    }
  }
}

const initPreview = () => {
  let shownModel: PersistentModel | null = null
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

  const modelUpdated = async (model: PersistentModel | null) => {
    if (model != null && R.equals(shownModel, model)) {
      // the returned model did not change, bail out
      return
    }
    shownModel = model
    if (model?.projectContents != null) {
      previewRender(model.projectContents)
    }
  }

  const handleModelUpdateEvent = (event: MessageEvent) => {
    // Received a model from the containing page.
    const eventData = event.data
    if (isPersistentModel(eventData)) {
      modelUpdated(eventData)
    }
  }

  const previewRender = async (projectContents: ProjectContents) => {
    const npmDependencies = dependenciesFromProjectContents(projectContents)
    let nodeModules = await fetchNodeModules(npmDependencies)

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
    const require = getRequireFn((modulesToAdd: NodeModules) => {
      // MUTATION
      Object.assign(nodeModules, modulesToAdd)
    }, nodeModules)

    // replacing the document body first
    const packageJson = projectContents['/package.json']
    if (packageJson != null && isCodeFile(packageJson)) {
      const parsedJSON = json5.parse(packageJson.fileContents)

      const utopiaSettings = R.path<any>(['utopia'], parsedJSON)
      const previewFileName = utopiaSettings['html']
      if (previewFileName != null) {
        const previewPath = `/${previewFileName}`
        const file = projectContents[previewPath]
        if (file != null && isCodeFile(file)) {
          try {
            try {
              ReactErrorOverlay.stopReportingRuntimeErrors()
            } catch (e) {
              // we don't care
            }
            const bodyContent = file.fileContents.split('<body>')[1].split('</body>')[0]
            document.body.innerHTML = bodyContent
            addOpenInUtopiaButton()
            ReactErrorOverlay.startReportingRuntimeErrors({})
          } catch (e) {
            console.warn(`no body found in html`, e)
          }
        }
      }

      const previewJsFileName = utopiaSettings['js']
      if (previewJsFileName != null) {
        const previewJSPath = `/${previewJsFileName}`
        const file = projectContents[previewJSPath]
        if (file == null || !isCodeFile(file)) {
          throw new Error(
            `Error processing the project files: the preview path (${previewJSPath}) did not point to a valid file`,
          )
        } else if (bundledProjectFiles[previewJSPath] == null) {
          throw new Error(
            `Error processing the project files: the build result does not contain the preview file: ${previewJSPath}`,
          )
        } else {
          /**
           * require the js entry point file which will evaluate the module.
           * the React entry point js file traditionally has a top level side effect,
           * calling ReactDOM.render() which starts the Preview app.
           */
          require('/', previewJSPath)
        }
      }
    }
  }

  const loadPreviewContent = () => {
    const projectId = getProjectID()
    window.addEventListener('message', handleModelUpdateEvent)

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
