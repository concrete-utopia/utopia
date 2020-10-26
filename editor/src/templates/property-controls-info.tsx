import * as R from 'ramda'
import { PropertyControlsInfo } from '../components/custom-code/code-file'
import {
  propertyControlsIFrameReady,
  updatePropertyControlsInfo,
} from '../components/editor/actions/actions'
import { dependenciesWithEditorRequirements } from '../components/editor/npm-dependency/npm-dependency'
import { initPropertyControlsProcessor } from '../core/property-controls/property-controls-processor'
import {
  createGetPropertyControlsInfoMessage,
  GetPropertyControlsInfoMessage,
} from '../core/property-controls/property-controls-utils'
import { applicative3Either, forEachRight } from '../core/shared/either'
import { NewBundlerWorker, RealBundlerWorker } from '../core/workers/bundler-bridge'
import { createBundle } from '../core/workers/bundler-promise'
import { objectKeyParser, parseAny, ParseResult } from '../utils/value-parser-utils'

// Not a full parse, just checks the primary fields are there.
function fastPropertyControlsParse(value: unknown): ParseResult<GetPropertyControlsInfoMessage> {
  return applicative3Either(
    createGetPropertyControlsInfoMessage,
    objectKeyParser(parseAny, 'exportsInfo')(value),
    objectKeyParser(parseAny, 'nodeModulesUpdate')(value),
    objectKeyParser(parseAny, 'projectContents')(value),
  )
}

const initPropertyControlsWorker = () => {
  const bundlerWorker = new NewBundlerWorker(new RealBundlerWorker())
  let lastMessage: GetPropertyControlsInfoMessage | null = null

  const onControlsProcessed = (propertyControlsInfo: PropertyControlsInfo) => {
    window.parent.postMessage(updatePropertyControlsInfo(propertyControlsInfo), '*')
  }

  const propertyControlsProcessor = initPropertyControlsProcessor(onControlsProcessed)

  const modelUpdated = async (model: GetPropertyControlsInfoMessage) => {
    if (!R.equals(lastMessage, model)) {
      const projectContents = model.projectContents
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

      const npmDependencies = dependenciesWithEditorRequirements(projectContents)
      propertyControlsProcessor(
        npmDependencies,
        model.nodeModulesUpdate,
        projectContents,
        bundledProjectFiles,
        model.exportsInfo,
      )
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

  const loadProjectValuesContent = () => {
    window.addEventListener('message', handleModelUpdateEvent)

    // Tell the editor we'd like to be sent the initial model.
    window.parent.postMessage(propertyControlsIFrameReady(), '*')
  }
  loadProjectValuesContent()
}

initPropertyControlsWorker()
