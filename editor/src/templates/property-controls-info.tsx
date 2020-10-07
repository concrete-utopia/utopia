import * as R from 'ramda'
import * as ReactErrorOverlay from 'react-error-overlay'
import {
  getExportValuesFromAllModules,
  incorporateBuildResult,
  processExportsInfo,
  PropertyControlsInfo,
} from '../components/custom-code/code-file'
import {
  propertyControlsIFrameReady,
  updatePropertyControlsInfo,
} from '../components/editor/actions/actions'
import { dependenciesWithEditorRequirements } from '../components/editor/npm-dependency/npm-dependency'
import { getRequireFn } from '../core/es-modules/package-manager/package-manager'
import { NodeModules } from '../core/shared/project-file-types'
import type { ProjectContents } from '../core/shared/project-file-types'
import { NewBundlerWorker, RealBundlerWorker } from '../core/workers/bundler-bridge'
import { createBundle } from '../core/workers/bundler-promise'
import {
  applyNodeModulesUpdate,
  createGetPropertyControlsInfoMessage,
  getControlsForExternalDependencies,
  GetPropertyControlsInfoMessage,
  NodeModulesUpdate,
} from '../core/property-controls/property-controls-utils'
import { fastForEach } from '../core/shared/utils'
import { ExportsInfo } from '../core/workers/ts/ts-worker'
import { PropertyControls } from 'utopia-api'
import { objectKeyParser, parseAny, ParseResult } from '../utils/value-parser-utils'
import { applicative3Either, forEachRight } from '../core/shared/either'
import { resolvedDependencyVersions } from '../core/third-party/third-party-components'
import { ProjectContentTreeRoot } from '../components/assets'

// Not a full parse, just checks the primary fields are there.
function fastPropertyControlsParse(value: unknown): ParseResult<GetPropertyControlsInfoMessage> {
  return applicative3Either(
    createGetPropertyControlsInfoMessage,
    objectKeyParser(parseAny, 'exportsInfo')(value),
    objectKeyParser(parseAny, 'nodeModulesUpdate')(value),
    objectKeyParser(parseAny, 'projectContents')(value),
  )
}

const initPropertyControls = () => {
  const bundlerWorker = new NewBundlerWorker(new RealBundlerWorker())

  let lastMessage: GetPropertyControlsInfoMessage | null = null
  let currentNodeModules: NodeModules = {}

  const modelUpdated = async (model: GetPropertyControlsInfoMessage) => {
    if (!R.equals(lastMessage, model)) {
      processPropertyControls(model.projectContents, model.nodeModulesUpdate, model.exportsInfo)
    }
  }

  const handleModelUpdateEvent = (event: MessageEvent) => {
    // Received a model from the containing page.
    const eventData = event.data
    const parsed = fastPropertyControlsParse(eventData)
    forEachRight(parsed, (model) => {
      modelUpdated(model)
    })
  }

  const processPropertyControls = async (
    projectContents: ProjectContentTreeRoot,
    nodeModulesUpdate: NodeModulesUpdate,
    exportsInfo: ReadonlyArray<ExportsInfo>,
  ) => {
    currentNodeModules = applyNodeModulesUpdate(currentNodeModules, nodeModulesUpdate)
    const npmDependencies = dependenciesWithEditorRequirements(projectContents)
    const resolvedNpmDependencies = resolvedDependencyVersions(npmDependencies, currentNodeModules)
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

    incorporateBuildResult(currentNodeModules, bundledProjectFiles)
    const requireFn = getRequireFn((modulesToAdd: NodeModules) => {
      // MUTATION
      Object.assign(currentNodeModules, modulesToAdd)
    }, currentNodeModules)

    const exportValues = getExportValuesFromAllModules(bundledProjectFiles, requireFn)

    let propertyControlsInfo: PropertyControlsInfo = getControlsForExternalDependencies(
      resolvedNpmDependencies,
    )
    fastForEach(exportsInfo, (result) => {
      const codeResult = processExportsInfo(exportValues[result.filename], result.exportTypes)
      let propertyControls: { [name: string]: PropertyControls } = {}
      if (codeResult.exports != null) {
        fastForEach(Object.keys(codeResult.exports), (name) => {
          const exportedObject = codeResult.exports[name].value
          if (exportedObject != null && exportedObject.propertyControls != null) {
            // FIXME validate shape
            propertyControls[name] = exportedObject.propertyControls
          }
        })
        const filenameNoExtension = result.filename.replace(/\.(js|jsx|ts|tsx)$/, '')
        propertyControlsInfo[filenameNoExtension] = propertyControls
      }
    })

    window.parent.postMessage(updatePropertyControlsInfo(propertyControlsInfo), '*')
  }

  const loadProjectValuesContent = () => {
    window.addEventListener('message', handleModelUpdateEvent)

    // Tell the editor we'd like to be sent the initial model.
    window.parent.postMessage(propertyControlsIFrameReady(), '*')
  }
  loadProjectValuesContent()
}
initPropertyControls()
