import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { UtopiaTsWorkers } from '../../../workers/common/worker-types'
import type { IGithubEndpoints } from '../endpoints'

export interface GithubOperationContext {
  fetch: (url: string, options: RequestInit) => Promise<Response>
  githubEndpoints: IGithubEndpoints
  updateProjectContentsWithParseResults: (
    workers: UtopiaTsWorkers,
    projectContents: ProjectContentTreeRoot,
  ) => Promise<ProjectContentTreeRoot>
}
