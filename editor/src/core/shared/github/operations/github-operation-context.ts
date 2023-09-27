import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { UtopiaTsWorkers } from '../../../workers/common/worker-types'

export interface GithubOperationContext {
  fetch: (url: string, options: RequestInit) => Promise<Response>
  updateProjectContentsWithParseResults: (
    workers: UtopiaTsWorkers,
    projectContents: ProjectContentTreeRoot,
  ) => Promise<ProjectContentTreeRoot>
}
