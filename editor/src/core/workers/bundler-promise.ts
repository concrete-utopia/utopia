import type { TypeDefinitions } from '../shared/npm-dependency-types'
import type { NewBundlerWorker } from './bundler-bridge'
import utils from '../../utils/utils'
import type { ProjectContentTreeRoot } from '../../components/assets'
import type { MultiFileBuildResult, OutgoingWorkerMessage } from './common/worker-types'

export function createBundle(
  worker: NewBundlerWorker,
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContentTreeRoot,
): Promise<{ buildResult: MultiFileBuildResult }> {
  return new Promise((resolve, reject) => {
    const jobID = utils.generateUUID()
    function bundleMessageHandler(bundleResultMessage: MessageEvent) {
      const data: OutgoingWorkerMessage = bundleResultMessage.data
      if (data.type === 'build' && data.jobID === jobID) {
        resolve({ buildResult: data.buildResult })
        worker.removeBundleResultEventListener(bundleMessageHandler)
      }
    }
    worker.addBundleResultEventListener(bundleMessageHandler)
    worker.sendInitMessage(typeDefinitions, projectContents, jobID)
  })
}
