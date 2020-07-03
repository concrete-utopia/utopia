import type { TypeDefinitions } from '../shared/npm-dependency-types'
import type { MultiFileBuildResult, OutgoingWorkerMessage } from './ts/ts-worker'
import type { ProjectContents } from '../shared/project-file-types'
import { NewBundlerWorker } from './bundler-bridge'

export function createBundle(
  worker: NewBundlerWorker,
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContents,
): Promise<{ buildResult: MultiFileBuildResult }> {
  return new Promise((resolve, reject) => {
    function bundleMessageHandler(bundleResultMessage: MessageEvent) {
      const data: OutgoingWorkerMessage = bundleResultMessage.data
      if (data.type === 'build') {
        resolve({ buildResult: data.buildResult })
        worker.removeBundleResultEventListener(bundleMessageHandler)
      }
    }
    worker.addBundleResultEventListener(bundleMessageHandler)
    worker.sendInitMessage(typeDefinitions, projectContents)
  })
}
