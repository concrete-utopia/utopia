import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { UtopiaTsWorkers } from '../../../workers/common/worker-types'
import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'

export interface GithubOperationContext {
  fetch: (url: string, options: RequestInit) => Promise<Response>
  updateProjectContentsWithParseResults: (
    workers: UtopiaTsWorkers,
    projectContents: ProjectContentTreeRoot,
  ) => Promise<ProjectContentTreeRoot>
}

export const OperationContext: GithubOperationContext = {
  fetch: (...args) => window.fetch(...args),
  updateProjectContentsWithParseResults: updateProjectContentsWithParseResults,
}
